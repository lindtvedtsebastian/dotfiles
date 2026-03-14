;;; bindings.el --- -*- lexical-binding: t -*-
;;; commentary:
;; bindings Emacs configurations
;;; code:

(defvar evil-want-keybinding nil) ; Must be disabled to ensure evil collection works

(require 'evil)
(require 'evil-collection)
(require 'evil-nerd-commenter)

(setq evil-want-fine-undo t)      ; Fine granular undo-redo
(evil-set-undo-system 'undo-redo) ; Use built-in undo-redo system
(evil-collection-init)            ; Initialize evil collection
(evil-mode)                       ; Enable evil mode
(evilnc-default-hotkeys)          ; Setup evil nerd commenter hotkeys
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

(with-eval-after-load 'lsp-mode
  (evil-define-key 'normal lsp-mode-map (kbd "g r") 'lsp-find-references))

(require 'scroll-on-drag)
(global-set-key (kbd "<down-mouse-2>") 'scroll-on-drag) ; Scroll on middle mouse button

;; Vertico
(require 'vertico)
(define-key vertico-map (kbd "C-j") 'vertico-next)
(define-key vertico-map (kbd "C-k") 'vertico-previous)

;; Corfu
(define-key read--expression-map (kbd "C-j") nil)
(global-set-key (kbd "C-j") 'corfu-next)
(global-set-key (kbd "C-k") 'corfu-previous)

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

(require 'consult)
(advice-add #'project-find-regexp :override #'consult-ripgrep)

;; Leader keys
(defvar leader-keys)                         ; Define new empty variable 'leader-keys'
(define-prefix-command 'leader-keys)         ; Create new keymap

(global-set-key (kbd "C-SPC") 'leader-keys)  ; Define global keybinding

;; Project keys
(defvar leader-keys/project)                 ; Define new empty variable for 'project' keys
(define-prefix-command 'leader-keys/project) ; Create new keymap for project
(define-key leader-keys (kbd "p") '("project" . leader-keys/project))
(define-key leader-keys/project (kbd "f") '("find file" . project-find-file))
(define-key leader-keys/project (kbd "d") '("find dir" . project-find-dir))
(define-key leader-keys/project (kbd "F") '("find regexp" . project-find-regexp))
(define-key leader-keys/project (kbd "s") '("switch project" . project-switch-project))
(define-key leader-keys/project (kbd "g") '("magit" . magit-project-status))
(define-key leader-keys/project (kbd "e") '("eshell" . project-eshell))
(define-key leader-keys/project (kbd "k") '("kill buffers" . project-kill-buffers))
;; lsp-mode
(defvar leader-keys/lsp)
(define-prefix-command 'leader-keys/lsp)
(define-key leader-keys (kbd "l") '("lsp" . leader-keys/lsp))
(define-key leader-keys/lsp (kbd "a") '("code action" . lsp-execute-code-action))
(define-key leader-keys/lsp (kbd "f") '("format buffer" . lsp-format-buffer))
(define-key leader-keys/lsp (kbd "r") '("rename" . lsp-rename))
(define-key leader-keys/lsp (kbd "d") '("find definition" . lsp-find-definition))
(define-key leader-keys/lsp (kbd "R") '("find references" . lsp-find-references))
(define-key leader-keys/lsp (kbd "i") '("find implementation" . lsp-find-implementation))
(define-key leader-keys/lsp (kbd "t") '("find type def" . lsp-find-type-definition))
(define-key leader-keys/lsp (kbd "h") '("describe thing" . lsp-describe-thing-at-point))
(define-key leader-keys/lsp (kbd "o") '("organize imports" . lsp-organize-imports))
(define-key leader-keys/lsp (kbd "s") '("symbols" . consult-imenu))
(define-key leader-keys/lsp (kbd "q") '("restart" . lsp-workspace-restart))

;; Embark
(require 'embark)
(require 'embark-consult)
(define-key leader-keys (kbd ".") '("embark act" . embark-act))
(define-key leader-keys (kbd ";") '("embark dwim" . embark-dwim))
(define-key minibuffer-local-map (kbd "C-c C-o") 'embark-export)
(define-key minibuffer-local-map (kbd "C-c C-l") 'embark-collect)

;; Recent files
(define-key leader-keys (kbd "r") '("recent files" . consult-recent-file))

;; Avy
(require 'avy)
(define-key leader-keys (kbd "j") '("jump char" . avy-goto-char-timer))
(define-key leader-keys (kbd "J") '("jump line" . avy-goto-line))

;; flycheck
(defvar leader-keys/flycheck)
(define-prefix-command 'leader-keys/flycheck)
(define-key leader-keys (kbd "c") '("flycheck" . leader-keys/flycheck))
(define-key leader-keys/flycheck (kbd "n") '("next error" . flycheck-next-error))
(define-key leader-keys/flycheck (kbd "p") '("prev error" . flycheck-previous-error))
(define-key leader-keys/flycheck (kbd "l") '("list errors" . flycheck-list-errors))

;; Buffer
(defvar leader-keys/buffer)
(define-prefix-command 'leader-keys/buffer)
(define-key leader-keys (kbd "b") '("buffer" . leader-keys/buffer))
(define-key leader-keys/buffer (kbd "b") '("switch" . consult-buffer))
(define-key leader-keys/buffer (kbd "k") '("kill" . kill-current-buffer))
(define-key leader-keys/buffer (kbd "s") '("scratch" . scratch-buffer))
(define-key leader-keys/buffer (kbd "m") '("messages" . (lambda () (interactive) (switch-to-buffer "*Messages*"))))
(define-key leader-keys/buffer (kbd "n") '("next" . next-buffer))
(define-key leader-keys/buffer (kbd "p") '("prev" . previous-buffer))
(define-key leader-keys/buffer (kbd "r") '("revert" . revert-buffer-quick))

;; Window
(defvar leader-keys/window)
(define-prefix-command 'leader-keys/window)
(define-key leader-keys (kbd "w") '("window" . leader-keys/window))
(define-key leader-keys/window (kbd "v") '("split right" . split-window-right))
(define-key leader-keys/window (kbd "s") '("split below" . split-window-below))
(define-key leader-keys/window (kbd "d") '("delete" . delete-window))
(define-key leader-keys/window (kbd "D") '("delete others" . delete-other-windows))
(define-key leader-keys/window (kbd "=") '("balance" . balance-windows))
(define-key leader-keys/window (kbd "h") '("move left" . windmove-left))
(define-key leader-keys/window (kbd "j") '("move down" . windmove-down))
(define-key leader-keys/window (kbd "k") '("move up" . windmove-up))
(define-key leader-keys/window (kbd "l") '("move right" . windmove-right))

;; treesit-fold
(defvar leader-keys/treesit-fold)
(define-prefix-command 'leader-keys/treesit-fold)
(define-key leader-keys (kbd "f") '("fold" . leader-keys/treesit-fold))
(define-key leader-keys/treesit-fold (kbd "o") '("open" . treesit-fold-open))
(define-key leader-keys/treesit-fold (kbd "O") '("open all" . treesit-fold-open-all))
(define-key leader-keys/treesit-fold (kbd "c") '("close" . treesit-fold-close))
(define-key leader-keys/treesit-fold (kbd "C") '("close" . treesit-fold-close-all))
;;; bindings.el ends here
