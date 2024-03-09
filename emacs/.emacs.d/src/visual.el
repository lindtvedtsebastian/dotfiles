;;; visual.el --- -*- lexical-binding: t -*-
;;; commentary:
;; visual Emacs configurations
;;; code:

(setq split-width-threshold 0) ; Reduce required width for splitting horizontally

(setq show-paren-delay 0)      ; No delay before highlighting parenthesis

(set-face-attribute 'default nil :font "Iosevka" :weight 'normal :width 'normal :height 140)

(set-frame-parameter nil 'alpha-background 80)

;;; visual.el ends here
