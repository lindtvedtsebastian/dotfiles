;;; dashboard.el --- -*- lexical-binding: t -*-
;;; commentary:
;; Custom dashboard buffer as base for all Emacs operations
;;; code:

(require 'recentf)
(require 'consult)
(require 'magit)
(require 'evil)

(defvar sl/dashboard--width 60
  "Width of the centered content block.")

;;; ----- Major Mode -----

(define-derived-mode sl/dashboard-mode special-mode "Dashboard"
  "Major mode for the dashboard buffer."
  (setq-local cursor-type nil)
  (hl-line-mode 1)
  (add-hook 'window-configuration-change-hook
            #'sl/dashboard--center-windows nil t))

(evil-set-initial-state 'sl/dashboard-mode 'normal)

;;; ----- Centering -----

(defun sl/dashboard--center (window)
  "Center dashboard content in WINDOW using margins."
  (let* ((win-width (window-total-width window))
         (margin (max 0 (/ (- win-width sl/dashboard--width) 2))))
    (set-window-margins window margin margin)))

(defun sl/dashboard--center-windows ()
  "Center dashboard in all windows showing the current buffer."
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (sl/dashboard--center win)))

(defun sl/dashboard--on-resize (frame)
  "Re-center dashboard windows in FRAME."
  (dolist (window (window-list frame))
    (when (eq (buffer-local-value 'major-mode (window-buffer window))
              'sl/dashboard-mode)
      (sl/dashboard--center window))))

(add-hook 'window-size-change-functions #'sl/dashboard--on-resize)

;;; ----- Render -----

(defun sl/dashboard-render ()
  "Render dashboard content into the current buffer."
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
    (sl/dashboard--shortcut "m" "magit"           'magit-status)
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
        (insert (propertize "  no known projects\n" 'face 'shadow))))

    (goto-char (point-min))))

(defun sl/dashboard-buffer ()
  "Create and return the dashboard buffer."
  (let ((buf (get-buffer-create "*dashboard*")))
    (with-current-buffer buf
      (unless (eq major-mode 'sl/dashboard-mode)
        (sl/dashboard-mode))
      (sl/dashboard-render))
    buf))

(defun sl/dashboard--shortcut (key desc _command)
  "Insert a shortcut line with KEY, DESC, and _COMMAND."
  (insert "  ")
  (insert (propertize key 'face 'font-lock-keyword-face))
  (insert (propertize (format "  %s" desc) 'face 'default))
  (insert "\n"))

;;; ----- Keybindings -----

(evil-define-key 'normal sl/dashboard-mode-map
  (kbd "r") #'consult-recent-file
  (kbd "f") #'find-file
  (kbd "p") #'project-switch-project
  (kbd "m") #'magit-status
  (kbd "q") #'quit-window)

(setq initial-buffer-choice #'sl/dashboard-buffer)

;;; dashboard.el ends here
