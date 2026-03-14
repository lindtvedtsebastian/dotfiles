;;; early-init.el --- -*- lexical-binding: t -*-
;;; commentary:
;;; pre-GUI initialization
;;; code:

(setq package-enable-at-startup nil)          ;; Disable package.el, will use straight.el
(setq site-run-file nil)                      ;; No site-wide run-time initialization
(setq inhibit-default-init t)                 ;; Inhibit default init from loading

(setenv "LSP_USE_PLISTS" "true")

;; GC tuning: disable during init, restore after
(setq gc-cons-threshold most-positive-fixnum)

;; Bypass file-name-handler-alist during init (speeds up file loads)
(defvar sl/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Suppress native-comp warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; Prevent frame resize during init
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; Disable UI elements early to prevent mid-init redraws
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;;; early-init.el ends here
