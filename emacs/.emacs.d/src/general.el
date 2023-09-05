;;; general.el --- -*- lexical-binding: t -*-
;;; commentary:
;;; general Emacs configurations
;;; code:

(global-auto-revert-mode)   ; Revert any file that changes on disk

(setq ns-pop-up-frames nil) ; File open requests opens in existing frame

(server-start)              ; Start emacs server

;; Don't report native comp errors
(setq comp-async-report-warnings-errors nil)
(setq native-comp-async-report-warnings-errors nil)

;; General UI configurations
(tool-bar-mode -1)                         ; Disable the toolbar
(set-fringe-mode 10)                       ; Set fringe size
(electric-pair-mode)                       ; Automatically pair characters
(scroll-bar-mode -1)                       ; Disable visible scrollbar
(column-number-mode)                       ; Enable column numbers in the modeline
(global-display-line-numbers-mode)         ; Show line numbers

(setq inhibit-startup-message t)           ; Hide startup message
(setq inhibit-startup-screen t)            ; Hide startup screen
(setq visible-bell t)                      ; Setup visible bell
(setq display-line-numbers-type 'relative) ; Set line number mode to be relative
(setq mouse-wheel-progressive-speed nil)   ; Disable scroll acceleration

;;; general.el ends here
