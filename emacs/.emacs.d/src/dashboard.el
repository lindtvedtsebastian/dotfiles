;;; dashboard.el --- -*- lexical-binding: t -*-
;;; commentary:
;; Custom dashboard buffer as base for all Emacs operations
;;; code:

(require 'recentf)
(require 'consult)
(require 'magit)
(require 'evil)

(defvar sl/dashboard--width 70
  "Width of the centered content block.")

(defvar-local sl/dashboard--top-overlay nil
  "Overlay used for vertical centering padding.")

(defvar-local sl/dashboard--logo-lines 0
  "Number of visual lines the logo image occupies.")

;;; ----- Major Mode -----

(define-derived-mode sl/dashboard-mode special-mode "Dashboard"
  "Major mode for the dashboard buffer."
  (setq-local cursor-type nil)
  (add-hook 'window-configuration-change-hook
            #'sl/dashboard--center-windows nil t))

(evil-set-initial-state 'sl/dashboard-mode 'normal)

;;; ----- Centering -----

(defun sl/dashboard--center (window)
  "Center dashboard content in WINDOW using margins and vertical padding."
  (with-current-buffer (window-buffer window)
    ;; Horizontal centering via margins
    (let* ((win-width (window-total-width window))
           (margin (max 0 (/ (- win-width sl/dashboard--width) 2))))
      (set-window-margins window margin margin))
    ;; Vertical centering via overlay
    (when sl/dashboard--top-overlay
      (let* ((buf-lines (count-lines (point-min) (point-max)))
             (visual-lines (+ buf-lines (max 0 (1- sl/dashboard--logo-lines))))
             (padding (max 0 (/ (- (window-body-height window) visual-lines) 2))))
        (overlay-put sl/dashboard--top-overlay 'before-string
                     (and (> padding 0) (make-string padding ?\n)))))))

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

;;; ----- Logo -----

(defun sl/dashboard--logo ()
  "Insert the Emacs logo centered in the content area."
  (let* ((logo-path (expand-file-name "emacs.png" user-emacs-directory))
         (max-h (* 7 (frame-char-height)))
         (image (create-image logo-path nil nil :max-height max-h)))
    (when image
      (setq sl/dashboard--logo-lines (ceiling (cdr (image-size image))))
      (let* ((img-width (car (image-size image t)))
             (content-px (* sl/dashboard--width (window-font-width)))
             (pad-chars (max 0 (/ (- content-px img-width) 2 (window-font-width)))))
        (insert (make-string pad-chars ?\s))
        (insert-image image)))))

;;; ----- Helpers -----

(defun sl/dashboard--center-text (text)
  "Return TEXT padded to center it within the content width."
  (let ((padding (max 0 (/ (- sl/dashboard--width (length text)) 2))))
    (concat (make-string padding ?\s) text)))

(defun sl/dashboard--shortcut-row (key1 desc1 key2 desc2)
  "Insert a row with two shortcuts: KEY1 DESC1 and KEY2 DESC2."
  (let* ((left-width (+ (length key1) 2 (length desc1)))
         (gap (max 1 (- 20 left-width))))
    (insert (propertize key1 'face '(:inherit font-lock-keyword-face :weight bold)))
    (insert (propertize (format "  %s" desc1) 'face 'shadow))
    (insert (make-string gap ?\s))
    (insert (propertize key2 'face '(:inherit font-lock-keyword-face :weight bold)))
    (insert (propertize (format "  %s" desc2) 'face 'shadow))
    (insert "\n")))

;;; ----- Render -----

(defun sl/dashboard-render ()
  "Render dashboard content into the current buffer."
  (let ((inhibit-read-only t)
        first-item)
    (erase-buffer)

    ;; Logo (GUI) or text fallback (terminal)
    (if (display-graphic-p)
        (sl/dashboard--logo)
      (insert (propertize (sl/dashboard--center-text "emacs")
                          'face '(:inherit font-lock-keyword-face :height 1.8 :weight bold))))
    (insert "\n\n\n")

    ;; Shortcut grid (2x2) - mark first item for cursor placement
    (setq first-item (point-marker))
    (sl/dashboard--shortcut-row "r" "recent" "f" "find file")
    (sl/dashboard--shortcut-row "p" "projects" "m" "magit")
    (insert "\n\n")

    ;; Recent files
    (insert (propertize (sl/dashboard--center-text "recent")
                        'face '(:inherit font-lock-keyword-face :height 1.1)))
    (insert "\n\n")
    (let ((recents (seq-take (seq-filter (lambda (f) (not (string-match-p "/straight/" f)))
                                         recentf-list)
                             10)))
      (if recents
          (dolist (file recents)
            (insert-text-button (file-name-nondirectory file)
                               'action (lambda (_) (find-file file))
                               'face 'default
                               'follow-link t
                               'help-echo (abbreviate-file-name file))
            (insert "\n"))
        (insert (propertize "no recent files yet\n" 'face 'shadow))))
    (insert "\n\n")

    ;; Projects
    (insert (propertize (sl/dashboard--center-text "projects")
                        'face '(:inherit font-lock-keyword-face :height 1.1)))
    (insert "\n\n")
    (let ((projects (project-known-project-roots)))
      (if projects
          (dolist (proj (seq-take projects 8))
            (let* ((path (abbreviate-file-name (directory-file-name proj)))
                   (name (file-name-nondirectory path))
                   (gap (max 1 (- 24 (length name)))))
              (insert-text-button name
                                 'action (lambda (_) (project-switch-project proj))
                                 'face 'default
                                 'follow-link t)
              (insert (propertize (concat (make-string gap ?\s) path) 'face 'shadow))
              (insert "\n")))
        (insert (propertize "no known projects\n" 'face 'shadow))))

    ;; Create overlay for vertical centering (updated by sl/dashboard--center)
    (setq sl/dashboard--top-overlay (make-overlay (point-min) (point-min)))

    (goto-char first-item)))

(defun sl/dashboard-buffer ()
  "Create and return the dashboard buffer."
  (let ((buf (get-buffer-create "*dashboard*")))
    (with-current-buffer buf
      (unless (eq major-mode 'sl/dashboard-mode)
        (sl/dashboard-mode))
      (sl/dashboard-render))
    buf))

;;; ----- Keybindings -----

(evil-define-key 'normal sl/dashboard-mode-map
  (kbd "r") #'consult-recent-file
  (kbd "f") #'find-file
  (kbd "p") #'project-switch-project
  (kbd "m") #'magit-status
  (kbd "q") #'quit-window)

(setq initial-buffer-choice #'sl/dashboard-buffer)

;;; dashboard.el ends here
