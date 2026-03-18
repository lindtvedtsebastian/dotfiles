;;; worktree.el --- -*- lexical-binding: t -*-
;;; commentary:
;; Git worktree integration with project.el and magit
;;; code:

(require 'project)
(require 'magit)

(defvar sl/worktree-parents-file
  (expand-file-name "worktree-parents" user-emacs-directory)
  "File listing directories that contain git worktrees.
One path per line.  Blank lines and lines starting with # are ignored.")

(defun sl/worktree-parents ()
  "Read worktree parent directories from `sl/worktree-parents-file'."
  (when (file-exists-p sl/worktree-parents-file)
    (with-temp-buffer
      (insert-file-contents sl/worktree-parents-file)
      (cl-loop for line in (split-string (buffer-string) "\n" t)
               for trimmed = (string-trim line)
               unless (or (string-empty-p trimmed)
                          (string-prefix-p "#" trimmed))
               collect trimmed))))

(defun sl/worktree-parents-save (parents)
  "Write PARENTS list to `sl/worktree-parents-file'."
  (with-temp-file sl/worktree-parents-file
    (insert "# Directories whose children are git worktree checkouts.\n")
    (insert "# One path per line. The directory name becomes the project name.\n")
    (dolist (p parents)
      (insert (abbreviate-file-name p) "\n"))))

(defun sl/worktree-parent-add (dir)
  "Add DIR as a worktree parent directory."
  (interactive "DWorktree parent directory: ")
  (let* ((dir (directory-file-name (expand-file-name dir)))
         (existing (mapcar #'expand-file-name (sl/worktree-parents))))
    (if (member dir existing)
        (message "Already a worktree parent: %s" (abbreviate-file-name dir))
      (sl/worktree-parents-save (append existing (list dir)))
      (message "Added worktree parent: %s" (abbreviate-file-name dir)))))

(defun sl/worktree-parent-remove ()
  "Remove a directory from the worktree parents list."
  (interactive)
  (let ((parents (sl/worktree-parents)))
    (unless parents
      (user-error "No worktree parents configured"))
    (let* ((selection (completing-read "Remove worktree parent: " parents nil t))
           (remaining (cl-remove (expand-file-name selection)
                                 (mapcar #'expand-file-name parents)
                                 :test #'string=)))
      (sl/worktree-parents-save remaining)
      (message "Removed worktree parent: %s" selection))))

(defun sl/git-worktrees (dir)
  "Return list of (path branch) pairs for git worktrees in DIR.
Returns nil if DIR is not a git repository."
  (let ((dir (expand-file-name dir)))
    (when (file-directory-p dir)
      (let ((output (with-temp-buffer
                      (when (= 0 (call-process "git" nil t nil
                                               "-C" dir
                                               "worktree" "list" "--porcelain"))
                        (buffer-string)))))
        (when output
          (let (result path branch)
            (dolist (line (split-string output "\n"))
              (cond
               ((string-prefix-p "worktree " line)
                (when path
                  (push (list path (or branch "detached")) result))
                (setq path (substring line 9)
                      branch nil))
               ((string-prefix-p "branch " line)
                (setq branch (replace-regexp-in-string
                              "^refs/heads/" "" (substring line 7))))))
            (when path
              (push (list path (or branch "detached")) result))
            (nreverse result)))))))

(defun sl/worktree-parent (dir)
  "Return the worktree parent for DIR if it is inside one, or nil."
  (let ((expanded (file-name-as-directory (expand-file-name dir))))
    (cl-find-if (lambda (parent)
                  (string-prefix-p (file-name-as-directory
                                    (expand-file-name parent))
                                   expanded))
                (sl/worktree-parents))))

(defun sl/project-prompt-dir ()
  "Prompt for a project directory with git worktree awareness.
For projects inside a declared worktree parent, the parent name is
used as the project name and all worktrees appear as separate
candidates with branch annotations."
  (let ((entries nil)
        (seen-parents (make-hash-table :test 'equal)))
    (dolist (root (project-known-project-roots))
      (let* ((clean-root (directory-file-name root))
             (parent (sl/worktree-parent clean-root)))
        (if parent
            (let ((exp-parent (expand-file-name parent)))
              (unless (gethash exp-parent seen-parents)
                (puthash exp-parent t seen-parents)
                (let ((name (file-name-nondirectory
                             (directory-file-name exp-parent)))
                      (worktrees (sl/git-worktrees clean-root)))
                  (if worktrees
                      (dolist (wt worktrees)
                        (push (list name
                                    (file-name-as-directory (nth 0 wt))
                                    (nth 1 wt))
                              entries))
                    (push (list name root nil) entries)))))
          (push (list (file-name-nondirectory clean-root) root nil)
                entries))))
    (setq entries (nreverse entries))
    (let* ((max-name (if entries
                         (apply #'max (mapcar (lambda (e) (length (nth 0 e)))
                                              entries))
                       0))
           (cand-to-path (make-hash-table :test 'equal))
           (candidates nil))
      (dolist (entry entries)
        (let* ((name (nth 0 entry))
               (path (nth 1 entry))
               (branch (nth 2 entry))
               (cand (if branch
                         (concat name
                                 (propertize
                                  (concat (make-string
                                           (- (+ max-name 4) (length name))
                                           ?\s)
                                          branch)
                                  'face 'shadow))
                       name)))
          (let ((base cand) (i 2))
            (while (gethash cand cand-to-path)
              (setq cand (format "%s<%d>" base i)
                    i (1+ i))))
          (push cand candidates)
          (puthash cand path cand-to-path)))
      (setq candidates (nreverse candidates))
      (let ((selected (completing-read "Project: " candidates nil t)))
        (or (gethash selected cand-to-path)
            selected)))))

(defun sl/worktree-create ()
  "Create a new git worktree with a new branch.
Places the worktree as a sibling of the current worktree directory."
  (interactive)
  (let* ((branch (magit-read-string-ns "Branch name"))
         (start (magit-read-starting-point "Start point"))
         (parent (file-name-directory
                  (directory-file-name (magit-toplevel))))
         (path (read-directory-name "Worktree directory: "
                                    parent nil nil branch)))
    (magit-worktree-branch path branch start)))

(defun sl/worktree-delete ()
  "Delete a secondary git worktree and kill its buffers."
  (interactive)
  (let* ((root (or (magit-toplevel)
                   (user-error "Not in a git repository")))
         (worktrees (sl/git-worktrees (directory-file-name root)))
         (secondary (cdr worktrees)))
    (unless secondary
      (user-error "No secondary worktrees to delete"))
    (let* ((choices (mapcar (lambda (wt)
                              (cons (format "%s [%s]"
                                            (abbreviate-file-name (nth 0 wt))
                                            (nth 1 wt))
                                    (nth 0 wt)))
                            secondary))
           (selection (completing-read "Delete worktree: " choices nil t))
           (path (cdr (assoc selection choices))))
      (dolist (buf (buffer-list))
        (let ((file (buffer-file-name buf)))
          (when (and file (string-prefix-p (file-name-as-directory path) file))
            (kill-buffer buf))))
      (magit-worktree-delete path))))

(setq project-prompter #'sl/project-prompt-dir)

(provide 'worktree)
;;; worktree.el ends here
