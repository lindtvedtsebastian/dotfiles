;;; visual.el --- -*- lexical-binding: t -*-
;;; commentary:
;; visual Emacs configurations
;;; code:

(setq show-paren-delay 0) ; No delay before highlighting parenthesis

(set-face-attribute 'default nil :font "Iosevka SS08" :weight 'normal :width 'normal :height 140)

(custom-set-faces
 '(mode-line ((t (:foreground "black" :background "white" :box (:line-width 6 :color "gray90"))))))

(require 'svg)
(require 'svg-lib)

(defvar mode-line-svg '(:eval (propertize ":3" 'display (svg-lib-tag (emacs-uptime) nil :foreground "white" :background "#673AB7" :alignment 1))))
(put 'mode-line-svg 'risky-local-variable t)

(setq mode-line-format '("%e" mode-line-front-space
			 (:propertize
			  ("" mode-line-mule-info mode-line-client mode-line-modified
			   mode-line-remote)
			  display (min-width (5.0)))
			 mode-line-frame-identification mode-line-buffer-identification "   "
			 mode-line-position evil-mode-line-tag (vc-mode vc-mode) "  "
			 mode-line-modes mode-line-svg mode-line-misc-info mode-line-end-spaces))

;;; visual.el ends here
