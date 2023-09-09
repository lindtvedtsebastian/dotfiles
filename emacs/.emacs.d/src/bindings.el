;;; bindings.el --- -*- lexical-binding: t -*-
;;; commentary:
;; bindings Emacs configurations
;;; code:

(defvar evil-want-keybinding nil) ; Must be disabled to ensure evil collection works

(require 'evil)
(require 'evil-collection)
(require 'evil-nerd-commenter)

(setq evil-want-fine-undo t)		; Fine granular undo-redo
(evil-set-undo-system 'undo-redo)	; Use built-in undo-redo system
(evil-collection-init)			; Initialize evil collection
(evil-mode)				; Enable evil mode
(evilnc-default-hotkeys)		; Setup evil nerd commenter hotkeys

;; Vertico
(require 'vertico)
(define-key vertico-map (kbd "C-j") 'vertico-next)
(define-key vertico-map (kbd "C-k") 'vertico-previous)

;; Consult
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-s") 'consult-line)

;; Project
(require 'project)
(setq project-switch-commands '((project-find-file "Find file")
				(project-find-regexp "Find regexp" "F")
				(project-find-dir "Find directory")
				(magit-project-status "Magit" "g")
				(project-eshell "Eshell")))

;; Leader keys
(defvar leader-keys)				; Define new empty variable 'leader-keys'
(define-prefix-command 'leader-keys)		; Create new keymap

(global-set-key (kbd "C-SPC") 'leader-keys)	; Define global keybinding

;; Project keys
(defvar leader-keys/project)			; Define new empty variable for 'project' keys
(define-prefix-command 'leader-keys/project)	; Create new keymap for project
(define-key leader-keys (kbd "p") '("project" . leader-keys/project))
(define-key leader-keys/project (kbd "f") '("find file" . project-find-file))
(define-key leader-keys/project (kbd "d") '("find dir" . project-find-dir))
(define-key leader-keys/project (kbd "F") '("find regexp" . project-find-regexp))
(define-key leader-keys/project (kbd "s") '("switch project" . project-switch-project))
(define-key leader-keys/project (kbd "g") '("magit" . magit-project-status))
(define-key leader-keys/project (kbd "e") '("eshell" . project-eshell))
(define-key leader-keys/project (kbd "k") '("kill buffers" . project-kill-buffers))

;; Org
(require 'org-roam)
(defvar leader-keys/org)			; Define new empty variable for 'org' keys
(define-prefix-command 'leader-keys/org)
(define-key leader-keys (kbd "o") '("org" . leader-keys/org))
(define-key leader-keys/org (kbd "a") '("agenda" . org-agenda))
(define-key leader-keys/org (kbd "c") '("capture" . org-capture))
(define-key leader-keys/org (kbd "f") '("find node" . org-roam-node-find))
(define-key leader-keys/org (kbd "i") '("insert node" . org-roam-node-insert))
(define-key leader-keys/org (kbd "t") '("add tag" . org-roam-tag-add))
(define-key leader-keys/org (kbd "r") '("add ref" . org-roam-ref-add))

;;; bindings.el ends here
