;;; dev.el --- -*- lexical-binding: t -*-
;;; commentary:
;; dev Emacs configurations
;;; code:

(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)



;; Magit
(require 'magit)
(setq magit-process-finish-apply-ansi-colors t)   ; Apply colors in Magit process
(setq transient-default-level 6)                  ; Show more transient commands

(require 'eglot)

;; Nix
(add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))
(add-hook 'nix-ts-mode-hook 'eglot-ensure)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))

;; Typescript / TSX
(require 'typescript-ts-mode)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)        ; Automatically start eglot in tsx-ts-mode
(add-hook 'typescript-ts-mode-hook 'eglot-ensure) ; Automatiaclly start eglot in typescript-ts-mode
(add-hook 'js-ts-mode-hook ' eglot-ensure)        ; Automatically start eglot in js-ts-mode
(add-hook 'js-jsx-mode-hook 'eglot-ensure)        ; Automatically start eglot in js-jsx

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))

;; YAML
(add-hook 'yaml-ts-mode-hook 'eglot-ensure)       ; Automatically start eglot in yaml-ts-mode

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

;; Rust
(add-to-list 'eglot-server-programs '((rust-ts-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
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
        (qmljs . ("~/dotfiles/dependencies/tree-sitter-qml"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

;; QML
(add-to-list 'load-path "~/dotfiles/dependencies/qml-ts-mode")
(require 'qml-ts-mode)
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-ts-mode))
(add-to-list 'eglot-server-programs '(qml-ts-mode . ("qmlls" "-E")))
(add-hook 'qml-ts-mode-hook 'eglot-ensure)

(require 'apheleia)
(apheleia-global-mode t)
(setq apheleia-formatters-respect-indent-level nil)

(add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt"))
(add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixpkgs-fmt))

;; Ensure flymake can read from load-path
(setq elisp-flymake-byte-compile-load-path load-path)
(add-hook 'prog-mode-hook 'flymake-mode)          ; Start flymake in all prog mode buffer
;;; dev.el ends here
