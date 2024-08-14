;;; env.el --- -*- lexical-binding: t -*-
;;; commentary:
;;; env variable configurations
;;; code:

(require 'exec-path-from-shell)

(exec-path-from-shell-initialize)

(require 'direnv)
(when (eq system-type 'gnu/linux)
  (direnv-mode))

;;; env.el ends here
