;;; dev.el --- -*- lexical-binding: t -*-
;;; commentary:
;; dev Emacs configurations
;;; code:

(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(require 'yasnippet)

(require 'lsp-mode)
(require 'lsp-completion)

(add-hook 'lsp-mode-hook 'lsp-diagnostics-mode)
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
(add-hook 'lsp-mode-hook 'lsp-completion-mode)
(add-hook 'lsp-mode-hook 'yas-minor-mode)

(setq lsp-completion-provider :none)
(setq lsp-completion-enable t)
(setq lsp-completion-enable-additional-text-edit t)

(require 'lsp-diagnostics)
(setq lsp-diagnostics-provider :flycheck)

(setq lsp-log-io nil)
(setq lsp-keep-workspace-alive nil)
(setq lsp-idle-delay 0.5)
(setq lsp-enable-xref t)
(setq lsp-auto-configure t)
(setq lsp-eldoc-enable-hover t)
(setq lsp-enable-dap-auto-configure t)
(setq lsp-enable-file-watchers nil)
(setq lsp-enable-folding t)
(setq lsp-enable-imenu t)
(setq lsp-enable-indentation t)
(setq lsp-enable-links nil)
(setq lsp-enable-on-type-formatting t)
(setq lsp-enable-suggest-server-download t)
(setq lsp-enable-symbol-highlighting t)
(setq lsp-enable-text-document-color nil)

(require 'lsp-ui)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-diagnostic-max-lines 20)
(setq lsp-ui-doc-use-childframe t)

(setq lsp-enable-snippet t)
(setq lsp-completion-show-kind t)

(require 'lsp-headerline)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-headerline-breadcrumb-enable-diagnostics nil)
(setq lsp-headerline-breadcrumb-enable-symbol-numbers nil)
(setq lsp-headerline-breadcrumb-icons-enable t)

(require 'lsp-modeline)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-modeline-workspace-status-enable nil)
(setq lsp-signature-doc-lines 1)
(setq lsp-eldoc-render-all nil)

(setq lsp-lens-enable t)
(setq lsp-semantic-tokens-enable nil)
(setq lsp-use-plists t)

(require 'flycheck)
(global-flycheck-mode)

(defvar lsp-tailwindcss-add-on-mode t)
(require 'lsp-tailwindcss)

;; Ensure flymake can read load-path in elisp
(setq elisp-flymake-byte-compile-load-path load-path)
(add-hook 'prog-mode-hook 'flymake-mode)          ; Start flymake in all prog mode buffers

;; Magit
(require 'magit)
(setq magit-process-finish-apply-ansi-colors t)   ; Apply colors in Magit process
(setq transient-default-level 6)                  ; Show more transient commands

;; Eglot
(require 'eglot)
(setq eglot-events-buffer-size 0)                 ; Disable event buffer

;; Nix
(add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))
(add-hook 'nix-ts-mode-hook 'lsp-deferred)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))

;; Typescript / TSX
(require 'typescript-ts-mode)
(add-hook 'tsx-ts-mode-hook 'lsp-deferred)        ; Automatically start eglot in tsx-ts-mode
(add-hook 'typescript-ts-mode-hook 'lsp-deferred) ; Automatiaclly start eglot in typescript-ts-mode
(add-hook 'js-ts-mode-hook ' lsp-deferred)        ; Automatically start eglot in js-ts-mode
(add-hook 'js-jsx-mode-hook 'lsp-deferred)        ; Automatically start eglot in js-jsx

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))

;; YAML
(add-hook 'yaml-ts-mode-hook 'lsp-deferred)       ; Automatically start eglot in yaml-ts-mode

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

;; Rust
(add-to-list 'eglot-server-programs '((rust-ts-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
(add-hook 'rust-ts-mode-hook 'lsp-deferred)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(defun connect-eglot (_interactive)
  "Dumb override for godot lsp connect."
  '("localhost" 6005))

(advice-add 'gdscript-eglot-contact :override #'connect-eglot)

;; Treesit
(require 'treesit)
(setq treesit-font-lock-level 4)
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "ocaml/src" "ocaml"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
        (c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (nix . ("https://github.com/nix-community/tree-sitter-nix"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

(require 'apheleia)
(apheleia-global-mode t)
(setq apheleia-formatters-respect-indent-level nil)
;;; dev.el ends here
