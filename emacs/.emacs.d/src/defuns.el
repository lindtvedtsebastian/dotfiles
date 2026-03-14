;;; defuns.el --- -*- lexical-binding: t -*-
;;; commentary:
;; Custom functions for package integration
;;; code:

(require 'lsp-mode)
(require 'cape)
(require 'flycheck)

;;; ----- LSP Completion -----

(defun corfu-lsp-setup ()
  "Configure lsp-mode for proper corfu/orderless use."
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))

(defun sl/friendly-lsp-completion-at-point ()
  "Wraps lsp-completion-at-point in cape-capf-super to make it friendly."
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     'lsp-completion-at-point
                     :with
                     'cape-keyword
                     'cape-abbrev
                     'cape-file))))

;;; ----- LSP Booster -----

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json, falling back to OLD-FN with ARGS."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD via OLD-FN.
TEST? skips the booster when non-nil."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let* ((command-from-exec-path (executable-find (car orig-result))))
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

;;; ----- Flycheck / Eldoc -----

(defun flycheck-eldoc (callback &rest _ignored)
  "Print flycheck messages at point by calling CALLBACK."
  (when-let* ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
    (mapc
     (lambda (err)
       (funcall callback
                (format "%s: %s"
                        (let ((level (flycheck-error-level err)))
                          (pcase level
                            ('info (propertize "I" 'face 'flycheck-error-list-info))
                            ('error (propertize "E" 'face 'flycheck-error-list-error))
                            ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                            (_ level)))
                        (flycheck-error-message err))
                :thing (or (flycheck-error-id err)
                           (flycheck-error-group err))
                :face 'font-lock-doc-face))
     flycheck-errors)))

(defun flycheck-prefer-eldoc ()
  "Add flycheck-eldoc to eldoc doc functions and disable default flycheck display."
  (add-hook 'eldoc-documentation-functions #'flycheck-eldoc -100 t)
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq flycheck-display-errors-function nil)
  (setq flycheck-help-echo-function nil))

;;; ----- SQL -----

(defvar cape-sql-keywords
  '("SERIAL" "PRIMARY KEY" "NOT NULL" "UNIQUE" "DEFAULT"
    "CHECK" "REFERENCES" "INT" "BIGINT" "TEXT" "BOOLEAN"
    "CREATE" "TABLE" "INSERT" "UPDATE" "DELETE" "VALUES"
    "JOIN" "ON DELETE" "ON UPDATE")
  "List of common ANSI SQL keywords for completion.")

(provide 'defuns)
;;; defuns.el ends here
