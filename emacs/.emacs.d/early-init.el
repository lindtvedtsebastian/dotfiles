;;; package --- emacs configuration
;;; commentary:
;;; code:

(setq package-enable-at-startup nil)          ;; Disable package.el, will use straight.el
(setq site-run-file nil)                      ;; No site-wide run-time initialization
(setq gc-cons-threshold most-positive-fixnum) ;; Set garbage collection threshold to bignum during init
(setq inhibit-default-init t)                 ;; Inhibit default init from loading

;; Add hook to reset garbage collection threshold after initialization is finished
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 32 1024 1024)))) ;; 32mb

;;; early-init.el ends here
