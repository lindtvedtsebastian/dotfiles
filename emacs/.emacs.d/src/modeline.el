;;; modeline.el --- -*- lexical-binding: t -*-
;;; commentary:
;; Custom modeline configuration
;;; code:

(require 'svg)
(require 'svg-lib)
(require 'evil)
(require 'vc-git)

;;; ----- Faces -----

(defgroup sl/mode-line nil
  "Custom mode-line faces."
  :group 'mode-line)

(defface sl/mode-line-evil-normal
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for evil normal state indicator."
  :group 'sl/mode-line)

(defface sl/mode-line-evil-insert
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for evil insert state indicator."
  :group 'sl/mode-line)

(defface sl/mode-line-evil-visual
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for evil visual state indicator."
  :group 'sl/mode-line)

(defface sl/mode-line-evil-emacs
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for evil emacs state indicator."
  :group 'sl/mode-line)

(defface sl/mode-line-modified
  '((t :inherit warning))
  "Face for buffer modified indicator."
  :group 'sl/mode-line)

(defface sl/mode-line-error
  '((t :inherit error :weight bold))
  "Face for flycheck error count."
  :group 'sl/mode-line)

(defface sl/mode-line-warning
  '((t :inherit warning :weight bold))
  "Face for flycheck warning count."
  :group 'sl/mode-line)

;;; ----- Segments -----

(defun sl/mode-line--evil-state ()
  "Return a string indicating the current evil state."
  (cond
   ((evil-normal-state-p)  (propertize " N " 'face 'sl/mode-line-evil-normal))
   ((evil-insert-state-p)  (propertize " I " 'face 'sl/mode-line-evil-insert))
   ((evil-visual-state-p)  (propertize " V " 'face 'sl/mode-line-evil-visual))
   ((evil-replace-state-p) (propertize " R " 'face 'sl/mode-line-evil-insert))
   ((evil-emacs-state-p)   (propertize " E " 'face 'sl/mode-line-evil-emacs))
   (t                      (propertize " ? " 'face 'shadow))))

(defun sl/mode-line--vc ()
  "Return git branch with icon, or nil if not in a repo."
  (when-let* ((file (buffer-file-name))
              (branch (and (vc-backend file)
                           (sl/mode-line--vc-branch-or-rev))))
    (let ((ml-fg (face-foreground 'mode-line nil t))
          (ml-bg (face-background 'mode-line nil t)))
      (list (propertize " " 'display
                        (svg-lib-icon "git-branch" nil :collection "octicons"
                                      :stroke 0 :scale 1 :padding 0
                                      :foreground (or ml-fg "black")
                                      :background (or ml-bg "white")))
            " "
            (propertize branch 'face 'magit-branch-local)))))

(defun sl/mode-line--vc-branch-or-rev ()
  "Return the current git branch name or short revision."
  (when-let* ((file (buffer-file-name))
              (backend (vc-backend file))
              (rev (vc-working-revision file backend)))
    (when (eq backend 'Git)
      (or (vc-git--symbolic-ref file)
          (substring rev 0 7)))))

(defun sl/mode-line--modified ()
  "Return a modified indicator or empty string."
  (if (and (buffer-modified-p) (buffer-file-name))
      (propertize " [+]" 'face 'sl/mode-line-modified)
    ""))

(defun sl/mode-line--major-mode ()
  "Return the major mode name."
  (propertize (format-mode-line mode-name) 'face 'shadow))

(defun sl/mode-line--flycheck ()
  "Return flycheck error and warning counts."
  (when (bound-and-true-p flycheck-mode)
    (let* ((counts (flycheck-count-errors flycheck-current-errors))
           (errors (or (cdr (assq 'error counts)) 0))
           (warnings (or (cdr (assq 'warning counts)) 0)))
      (when (> (+ errors warnings) 0)
        (list " "
              (propertize (format "E:%d" errors) 'face 'sl/mode-line-error)
              " "
              (propertize (format "W:%d" warnings) 'face 'sl/mode-line-warning))))))

(defun sl/mode-line--position ()
  "Return line:column position."
  (propertize " %l:%c " 'face 'shadow))

;;; ----- Assembly -----

(defvar-local sl/mode-line-evil    '(:eval (sl/mode-line--evil-state)))
(defvar-local sl/mode-line-vc      '(:eval (sl/mode-line--vc)))
(defvar-local sl/mode-line-modified '(:eval (sl/mode-line--modified)))
(defvar-local sl/mode-line-mode    '(:eval (sl/mode-line--major-mode)))
(defvar-local sl/mode-line-flycheck '(:eval (sl/mode-line--flycheck)))
(defvar-local sl/mode-line-position '(:eval (sl/mode-line--position)))

(put 'sl/mode-line-evil 'risky-local-variable t)
(put 'sl/mode-line-vc 'risky-local-variable t)
(put 'sl/mode-line-modified 'risky-local-variable t)
(put 'sl/mode-line-mode 'risky-local-variable t)
(put 'sl/mode-line-flycheck 'risky-local-variable t)
(put 'sl/mode-line-position 'risky-local-variable t)

(setq-default mode-line-format
              '("%e"
                sl/mode-line-evil
                sl/mode-line-vc
                " "
                mode-line-buffer-identification
                sl/mode-line-modified
                "  "
                sl/mode-line-mode
                sl/mode-line-flycheck
                mode-line-format-right-align
                sl/mode-line-position))

;;; modeline.el ends here
