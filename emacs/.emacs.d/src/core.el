;;; core.el --- -*- lexical-binding: t -*-
;;; commentary:
;;; core Emacs configurations
;;; code:

(global-auto-revert-mode)                    ; Revert any file that changes on disk

(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))               ; File open requests opens in existing frame
(setq pop-up-windows nil)                    ; Don't open buffers in new windows

(require 'server)
(unless (server-running-p)
  (server-start))                            ; Start emacs server

;; General UI configurations
(setq inhibit-startup-message t)             ; Hide startup message
(setq inhibit-startup-screen t)              ; Hide startup screen
(setq visible-bell t)                        ; Setup visible bell
(setq mouse-wheel-progressive-speed t)       ; Disable scroll acceleration
(defvar display-line-numbers-type 'relative) ; Set line number mode to be relative

(tool-bar-mode -1)                           ; Disable the toolbar
(set-fringe-mode 10)                         ; Set fringe size
(electric-pair-mode)                         ; Automatically pair characters
(scroll-bar-mode -1)                         ; Disable visible scrollbar
(column-number-mode)                         ; Enable column numbers in the modeline

;; Remove jsonrpc event hook for performance increase...
(require 'jsonrpc)
(setq jsonrpc-event-hook nil)

;; Customize TAB behaviour
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Encoding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; Set custom file location and load it if it exists.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil t))

;; Set custom theme directory
(setq custom-theme-directory (concat user-emacs-directory "themes/"))

;; General backup configs
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups/")))) ; Backup directory
(setq make-backup-files t)                                  ; Backup of a file the first time it is saved
(setq vc-make-backup-files t)                               ; No backup of files under version control
(setq backup-by-copying t)                                  ; Do not use symlinks for backup
(setq version-control t)                                    ; Use version control numbers for backup files
(setq delete-old-versions t)                                ; Clean up excess backup files
(setq kept-old-versions 5)                                  ; Number of old versions to keep
(setq kept-new-versions 5)                                  ; Number of new version to keep
(setq delete-by-moving-to-trash t)                          ; Delete files to system trash can

;; Dired
(put 'dired-find-alternate-file 'disabled nil) ; Allow re-using dired buffers

;;; core.el ends here
