;;; email.el --- Email configurations  -*- lexical-binding: t; -*-
;;; Commentary:
;; 
;;; Code:

(when (eq system-type 'darwin)
  (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/"))

(require 'mu4e)

(setq mu4e-mu-binary (executable-find "mu"))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-update-interval (* 5 60))

(provide 'email)

;;; email.el ends here
