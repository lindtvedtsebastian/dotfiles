;;; visual.el --- -*- lexical-binding: t -*-
;;; commentary:
;; visual Emacs configurations
;;; code:

(setq split-width-threshold 80) ; Prefer side-by-side splits, allow vertical on narrow frames

(setq show-paren-delay 0)      ; No delay before highlighting parenthesis

(set-face-attribute 'default nil :font "Iosevka" :weight 'normal :width 'normal :height 120)

(set-frame-parameter nil 'alpha-background 80)

;; Frame size: ~50% of screen, centered
(when (display-graphic-p)
  (when-let* ((attrs (frame-monitor-attributes))
              (workarea (alist-get 'workarea attrs)))
    (let* ((x (nth 0 workarea))
           (y (nth 1 workarea))
           (sw (nth 2 workarea))
           (sh (nth 3 workarea))
           (fw (/ sw 2))
           (fh (round (* sh 0.75))))
      (set-frame-size (selected-frame) fw fh t)
      (set-frame-position (selected-frame)
                          (+ x (/ (- sw fw) 2))
                          (+ y (/ (- sh fh) 2))))))

(setq dired-listing-switches "-lah")

(require 'colorful-mode)
(global-colorful-mode)

;; Ligatures
(require 'ligature)
(ligature-set-ligatures 'prog-mode '("==" "!=" ">=" "<=" "&&" "||"
                                      "->" "=>" "::" "<-" ".."
                                      ">>=" "<<=" "<=>" "=/="
                                      "++" "--" "**" "//" "/*" "*/"))
(global-ligature-mode)

;; Pulse on jump
(require 'pulse)
(setq pulse-delay 0.04)
(setq pulse-iterations 6)
(dolist (cmd '(evil-scroll-up evil-scroll-down
               evil-scroll-page-up evil-scroll-page-down
               evil-goto-line evil-goto-first-line
               recenter-top-bottom other-window
               windmove-left windmove-right windmove-up windmove-down))
  (advice-add cmd :after (lambda (&rest _) (pulse-momentary-highlight-one-line (point)))))

(load-theme 'nimbus)
;;; visual.el ends here
