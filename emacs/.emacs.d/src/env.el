;;; env.el --- -*- lexical-binding: t -*-
;;; commentary:
;;; env variable configurations
;;; code:

(require 'exec-path-from-shell)

(exec-path-from-shell-initialize)

(when (eq 'system-type "gnu/linux")
  ((require 'direnv)
   (direnv-mode)))


;;; env.el ends here
