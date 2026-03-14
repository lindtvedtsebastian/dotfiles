;;; visual.el --- -*- lexical-binding: t -*-
;;; commentary:
;; visual Emacs configurations
;;; code:

(setq split-width-threshold 80) ; Prefer side-by-side splits, allow vertical on narrow frames

(setq show-paren-delay 0)      ; No delay before highlighting parenthesis

(set-face-attribute 'default nil :font "Iosevka" :weight 'normal :width 'normal :height 120)

(set-frame-parameter nil 'alpha-background 80)

(setq dired-listing-switches "-lah")

(require 'colorful-mode)
(global-colorful-mode)

(load-theme 'nimbus)
;;; visual.el ends here
