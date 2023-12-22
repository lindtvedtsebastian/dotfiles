;;; modeline.el --- -*- lexical-binding: t -*-
;;; commentary:
;; modeline Emacs configurations
;;; code:

(custom-set-faces
 '(mode-line ((t (:foreground "black" :background "gray90" :box (:line-width 6 :color "gray90"))))))

(require 'svg)
(require 'svg-lib)

(defvar mode-line-svg '(:eval (propertize ":3" 'display (svg-lib-tag (emacs-uptime) nil :foreground "white" :background "#673AB7" :alignment 1))))
(put 'mode-line-svg 'risky-local-variable t)

(defvar-local sl/mode-line-vc '(:eval (list
                                       (propertize ":3" 'display
                                                   (svg-lib-icon "git-branch" nil :collection "octicons"
                                                                 :stroke 0 :scale 1 :padding 0 :foreground "black" :background "gray90"))
                                       " "
                                       (propertize (sl/mode-line--vc-branch-or-rev) 'face 'keyword))))
  (put 'sl/mode-line-vc 'risky-local-variable t)

(kill-local-variable 'mode-line-format)
(force-mode-line-update)

(setq-default mode-line-format `("%e" sl/mode-line-vc " " mode-line-buffer-identification))

(require 'vc-git)

(defun sl/mode-line--vc-branch-or-rev ()
  "The current git branch or revision."
  (let* ((file (buffer-file-name))
         (backend (vc-backend file))
         (rev (vc-working-revision file backend)))
    (when (eq backend 'Git) (or (vc-git--symbolic-ref file)
                                (substring rev 0 7)))))



;;; modeline.el ends here
