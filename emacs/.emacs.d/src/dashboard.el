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
  (setq-local fill-column sl/dashboard--width)
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

(defun sl/dashboard--block-pad (width)
  "Return padding string to center a block of WIDTH within the content area."
  (make-string (max 0 (/ (- sl/dashboard--width width) 2)) ?\s))

;;; ----- Navigation -----

(defun sl/dashboard-next-item ()
  "Move to the next item line."
  (interactive)
  (let ((found nil))
    (save-excursion
      (goto-char (line-end-position))
      (while (and (not found) (not (eobp)))
        (forward-line 1)
        (when (get-text-property (point) 'sl/dashboard-item)
          (setq found (point)))))
    (when found (goto-char found))))

(defun sl/dashboard-prev-item ()
  "Move to the previous item line."
  (interactive)
  (let ((found nil))
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (when (get-text-property (point) 'sl/dashboard-item)
          (setq found (point)))))
    (when found (goto-char found))))

(defun sl/dashboard-next-section ()
  "Move to the first item of the next section."
  (interactive)
  (let ((found nil))
    (save-excursion
      (forward-line 1)
      (while (and (not found) (not (eobp)))
        (when (get-text-property (point) 'sl/dashboard-section)
          (setq found (point)))
        (forward-line 1)))
    (when found
      (goto-char found)
      (sl/dashboard-next-item))))

(defun sl/dashboard-prev-section ()
  "Move to the first item of the previous section."
  (interactive)
  (let ((found nil)
        (seen-item nil))
    (save-excursion
      ;; Skip backward past current section's items to find the header
      (beginning-of-line)
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (when (get-text-property (point) 'sl/dashboard-item)
          (setq seen-item t))
        (when (and seen-item (get-text-property (point) 'sl/dashboard-section))
          (setq found (point)))))
    (when found
      (goto-char found)
      (sl/dashboard-next-item))))

(defun sl/dashboard-press ()
  "Activate the button on the current line."
  (interactive)
  (let ((eol (line-end-position))
        (pos (line-beginning-position)))
    (while (and (< pos eol) (not (button-at pos)))
      (setq pos (1+ pos)))
    (when (button-at pos)
      (button-activate (button-at pos)))))

(defun sl/dashboard-refresh ()
  "Re-render the dashboard."
  (interactive)
  (sl/dashboard-render)
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (sl/dashboard--center win)))

;;; ----- Render -----

(defun sl/dashboard-render ()
  "Render dashboard content into the current buffer."
  (let ((inhibit-read-only t)
        first-item)
    (erase-buffer)

    ;; Logo (GUI) or text fallback (terminal)
    (if (display-graphic-p)
        (sl/dashboard--logo)
      (insert (propertize "emacs" 'face '(:inherit font-lock-keyword-face :height 1.8 :weight bold)))
      (center-line))
    (insert "\n\n\n")

    ;; Shortcut grid (2x2, centered as a block with aligned columns)
    (let* ((col2 17)
           (grid-w (+ col2 12))
           (pad (sl/dashboard--block-pad grid-w)))
      (dolist (row '(("r" "recent" "f" "find file")
                     ("p" "projects" "m" "magit")))
        (let* ((k1 (nth 0 row)) (d1 (nth 1 row))
               (k2 (nth 2 row)) (d2 (nth 3 row))
               (gap (max 1 (- col2 (+ (length k1) 2 (length d1))))))
          (insert pad)
          (insert (propertize k1 'face '(:inherit font-lock-keyword-face :weight bold)))
          (insert (propertize (format "  %s" d1) 'face 'shadow))
          (insert (make-string gap ?\s))
          (insert (propertize k2 'face '(:inherit font-lock-keyword-face :weight bold)))
          (insert (propertize (format "  %s" d2) 'face 'shadow))
          (insert "\n"))))
    (insert "\n\n")

    ;; Recent files
    (let ((beg (point)))
      (insert (propertize "recent" 'face '(:inherit font-lock-keyword-face :height 1.1)))
      (center-line)
      (put-text-property beg (1+ beg) 'sl/dashboard-section t))
    (insert "\n\n")
    (let ((recents (seq-take (seq-filter (lambda (f) (not (string-match-p "/straight/" f)))
                                         recentf-list)
                             10)))
      (if recents
          (let* ((entries (mapcar (lambda (f)
                                   (let ((name (file-name-nondirectory f))
                                         (dir (abbreviate-file-name (file-name-directory f))))
                                     (list name dir f)))
                                 recents))
                 (max-name (apply #'max (mapcar (lambda (e) (length (car e))) entries)))
                 (max-row (apply #'max
                                 (mapcar (lambda (e) (+ max-name 4 (length (nth 1 e))))
                                         entries)))
                 (pad (sl/dashboard--block-pad max-row)))
            (dolist (entry entries)
              (let ((beg (point))
                    (name (nth 0 entry))
                    (dir (nth 1 entry))
                    (file (nth 2 entry))
                    (name-gap (make-string (- (+ max-name 4) (length (nth 0 entry))) ?\s)))
                (unless first-item (setq first-item (copy-marker beg)))
                (insert pad)
                (insert-text-button name
                                   'action (lambda (_) (find-file file))
                                   'face 'default
                                   'follow-link t)
                (insert (propertize (concat name-gap dir) 'face 'shadow))
                (put-text-property beg (point) 'sl/dashboard-item t)
                (insert "\n"))))
        (insert (propertize "no recent files yet" 'face 'shadow))
        (center-line)
        (insert "\n")))
    (insert "\n\n")

    ;; Projects
    (let ((beg (point)))
      (insert (propertize "projects" 'face '(:inherit font-lock-keyword-face :height 1.1)))
      (center-line)
      (put-text-property beg (1+ beg) 'sl/dashboard-section t))
    (insert "\n\n")
    (let ((projects (project-known-project-roots)))
      (if projects
          (let* ((entries (mapcar (lambda (proj)
                                   (let* ((path (abbreviate-file-name (directory-file-name proj)))
                                          (name (file-name-nondirectory path)))
                                     (list name path proj)))
                                 (seq-take projects 8)))
                 (max-name (apply #'max (mapcar (lambda (e) (length (car e))) entries)))
                 (max-row (apply #'max
                                 (mapcar (lambda (e) (+ max-name 4 (length (nth 1 e))))
                                         entries)))
                 (pad (sl/dashboard--block-pad max-row)))
            (dolist (entry entries)
              (let ((beg (point))
                    (name (nth 0 entry))
                    (path (nth 1 entry))
                    (proj (nth 2 entry))
                    (name-gap (make-string (- (+ max-name 4) (length (nth 0 entry))) ?\s)))
                (unless first-item (setq first-item (copy-marker beg)))
                (insert pad)
                (insert-text-button name
                                   'action (lambda (_) (project-switch-project proj))
                                   'face 'default
                                   'follow-link t)
                (insert (propertize (concat name-gap path) 'face 'shadow))
                (put-text-property beg (point) 'sl/dashboard-item t)
                (insert "\n"))))
        (insert (propertize "no known projects" 'face 'shadow))
        (center-line)
        (insert "\n")))

    ;; Vertical centering overlay (delete old one first to avoid stacking)
    (when sl/dashboard--top-overlay
      (delete-overlay sl/dashboard--top-overlay))
    (setq sl/dashboard--top-overlay (make-overlay (point-min) (point-min)))

    ;; Cursor on first item
    (goto-char (or first-item (point-min)))))

(defun sl/dashboard-buffer ()
  "Create and return the dashboard buffer."
  (let ((buf (get-buffer-create "*dashboard*")))
    (with-current-buffer buf
      (unless (eq major-mode 'sl/dashboard-mode)
        (sl/dashboard-mode))
      (sl/dashboard-render))
    buf))

(defun sl/dashboard-open ()
  "Switch to the dashboard buffer, creating it if needed."
  (interactive)
  (switch-to-buffer (sl/dashboard-buffer)))

;;; ----- Keybindings -----

(evil-define-key 'normal sl/dashboard-mode-map
  (kbd "j")       #'sl/dashboard-next-item
  (kbd "k")       #'sl/dashboard-prev-item
  (kbd "TAB")     #'sl/dashboard-next-section
  (kbd "<backtab>") #'sl/dashboard-prev-section
  (kbd "RET")     #'sl/dashboard-press
  (kbd "o")       #'sl/dashboard-press
  (kbd "g r")     #'sl/dashboard-refresh
  (kbd "r")       #'consult-recent-file
  (kbd "f")       #'find-file
  (kbd "p")       #'project-switch-project
  (kbd "m")       #'magit-status
  (kbd "q")       #'quit-window)

(setq initial-buffer-choice #'sl/dashboard-buffer)

;;; dashboard.el ends here
