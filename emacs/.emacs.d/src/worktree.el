;;; worktree.el --- -*- lexical-binding: t -*-
;;; commentary:
;; Git worktree integration with project.el and magit
;;; code:

(require 'project)
(require 'magit)

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

(defun sl/project-prompt-dir ()
  "Prompt for a project directory with git worktree awareness.
For any known project that has git worktrees, all worktrees appear
as separate candidates with branch annotations."
  (let ((entries nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (root (project-known-project-roots))
      (let* ((clean-root (directory-file-name root))
             (name (file-name-nondirectory clean-root)))
        (when (file-directory-p clean-root)
          (let ((worktrees (sl/git-worktrees clean-root)))
            (if (and worktrees (> (length worktrees) 1))
                (let ((main-root (expand-file-name (nth 0 (car worktrees)))))
                  (unless (gethash main-root seen)
                    (puthash main-root t seen)
                    (dolist (wt worktrees)
                      (push (list (file-name-nondirectory
                                   (directory-file-name main-root))
                                  (file-name-as-directory (nth 0 wt))
                                  (nth 1 wt))
                            entries))))
              (push (list name root nil) entries))))))
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

(setq project-prompter #'sl/project-prompt-dir)

(provide 'worktree)
;;; worktree.el ends here
