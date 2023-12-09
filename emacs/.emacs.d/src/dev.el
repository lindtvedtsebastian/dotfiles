;;; dev.el --- -*- lexical-binding: t -*-
;;; commentary:
;; dev Emacs configurations
;;; code:

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

;; Typescript / TSX
(require 'typescript-ts-mode)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)        ; Automatically start eglot in tsx-ts-mode
(add-hook 'typescript-ts-mode-hook 'eglot-ensure) ; Automatiaclly start eglot in typescript-ts-mode


(defun connect-eglot (_interactive)
  "Dumb override for godot lsp connect."
  '("localhost" 6005))

(advice-add 'gdscript-eglot-contact :override #'connect-eglot)

;; Treesit
(require 'treesit)
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
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

(require 'apheleia-core)
(apheleia-global-mode t)
;;; dev.el ends here
