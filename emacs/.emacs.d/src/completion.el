;;; completion.el --- -*- lexical-binding: t -*-
;;; commentary:
;; completion Emacs configurations
;;; code:

(require 'vertico)

(setq vertico-cycle t)	; Enable cycling when using vertico-next & vertico-previous
(vertico-mode)		; Enable vertico

(require 'marginalia)
(marginalia-mode)

(require 'orderless)
(setq completion-styles '(orderless partial-completion basic))

(require 'corfu)
(setq corfu-cycle t)		; Cycle corfu candidates
(setq corfu-auto t)		; Trigger corfu automatically
(setq corfu-auto-prefix 2)	; Required prefixes for autocompletion
(setq corfu-auto-delay 0)	; Delay before triggering autocompletion
(global-corfu-mode)		; Enable corfu globally

(require 'kind-icon)
(setq kind-icon-default-face 'corfu-default)				; Use corfu colors for icons
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)	; Set kind-icon as default margin formatter

(require 'which-key)
(setq which-key-idle-delay 0) ; Do not wait to display which key buffer
(which-key-mode)
;;; completion.el ends here
