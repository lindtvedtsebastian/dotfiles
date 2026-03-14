;;; dashboard.el --- -*- lexical-binding: t -*-
;;; commentary:
;; Custom dashboard buffer as base for all Emacs operations
;;; code:

(require 'recentf)
(require 'consult)
(require 'magit)
(require 'evil)

(defun sl/dashboard-buffer ()
  "Create and return the dashboard buffer."
  (let ((buf (get-buffer-create "*dashboard*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)

        ;; Header
        (insert (propertize "emacs" 'face '(:inherit font-lock-keyword-face :height 1.8 :weight bold)))
        (insert "\n\n")

        ;; Shortcuts
        (insert (propertize "shortcuts" 'face '(:inherit shadow :height 1.1)))
        (insert "\n\n")
        (sl/dashboard--shortcut "r" "recent files"   'consult-recent-file)
        (sl/dashboard--shortcut "f" "find file"       'find-file)
        (sl/dashboard--shortcut "p" "projects"        'project-switch-project)
        (sl/dashboard--shortcut "g" "magit"           'magit-status)
        (insert "\n")

        ;; Recent files
        (insert (propertize "recent files" 'face '(:inherit shadow :height 1.1)))
        (insert "\n\n")
        (let ((recents (seq-take (seq-filter (lambda (f) (not (string-match-p "/straight/" f)))
                                             recentf-list)
                                 10)))
          (if recents
              (dolist (file recents)
                (let ((short (abbreviate-file-name file)))
                  (insert "  ")
                  (insert-text-button short
                                     'action (lambda (_) (find-file file))
                                     'face 'link
                                     'follow-link t)
                  (insert "\n")))
            (insert (propertize "  no recent files yet\n" 'face 'shadow))))
        (insert "\n")

        ;; Projects
        (insert (propertize "projects" 'face '(:inherit shadow :height 1.1)))
        (insert "\n\n")
        (let ((projects (project-known-project-roots)))
          (if projects
              (dolist (proj (seq-take projects 8))
                (let ((name (abbreviate-file-name proj)))
                  (insert "  ")
                  (insert-text-button name
                                     'action (lambda (_) (project-switch-project proj))
                                     'face 'link
                                     'follow-link t)
                  (insert "\n")))
            (insert (propertize "  no known projects\n" 'face 'shadow)))))

      ;; Keymap
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "r") #'consult-recent-file)
      (local-set-key (kbd "f") #'find-file)
      (local-set-key (kbd "p") #'project-switch-project)
      (local-set-key (kbd "g") #'magit-status)
      (local-set-key (kbd "q") #'kill-buffer-and-window)

      (setq-local buffer-read-only t)
      (setq-local cursor-type nil)
      (evil-emacs-state)
      (goto-char (point-min)))
    buf))

(defun sl/dashboard--shortcut (key desc _command)
  "Insert a shortcut line with KEY, DESC, and _COMMAND."
  (insert "  ")
  (insert (propertize key 'face 'font-lock-keyword-face))
  (insert (propertize (format "  %s" desc) 'face 'default))
  (insert "\n"))

(setq initial-buffer-choice #'sl/dashboard-buffer)

;;; dashboard.el ends here
