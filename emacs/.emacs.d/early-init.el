;;; package --- emacs configuration
;;; commentary:
;;; code:

(setq package-enable-at-startup nil)          ;; Disable package.el, will use straight.el
(setq site-run-file nil)                      ;; No site-wide run-time initialization
(setq inhibit-default-init t)                 ;; Inhibit default init from loading

(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here
