;;; dev.el --- -*- lexical-binding: t -*-
;;; commentary:
;; dev Emacs configurations
;;; code:

;;; ----- Editor Basics -----

(setq read-process-output-max (* 1024 1024))
(setq process-adaptive-read-buffering nil)

(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; ----- LSP -----

(require 'lsp-mode)
(require 'lsp-completion)
(require 'lsp-icons)
(require 'lsp-headerline)

(setq lsp-keymap-prefix "C-c l")
(setq lsp-completion-provider :none)
(setq lsp-log-io nil)
(setq lsp-keep-workspace-alive nil)
(setq lsp-idle-delay 0.5)
(setq lsp-enable-xref t)
(setq lsp-enable-imenu t)
(setq lsp-enable-file-watchers t)
(setq lsp-file-watch-threshold 1000)
(setq lsp-eldoc-enable-hover t)
(setq lsp-enable-folding t)
(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-enable-suggest-server-download t)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-text-document-color nil)
(setq lsp-completion-enable t)
(setq lsp-completion-enable-additional-text-edit t)
(setq lsp-enable-snippet t)
(setq lsp-completion-show-kind t)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-headerline-breadcrumb-icons-enable t)
(setq lsp-headerline-breadcrumb-enable-symbol-numbers nil)
(setq lsp-headerline-breadcrumb-enable-diagnostics nil)
(setq lsp-semantic-tokens-enable nil)
(setq lsp-use-plists t)
(setq lsp-eldoc-render-all t)
(setq lsp-inlay-hint-enable t)

;; Corfu integration
(defun corfu-lsp-setup ()
  "Configure lsp-mode for proper corfu/orderless use."
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-completion-mode-hook #'corfu-lsp-setup)

;; Cape integration
(require 'cape)
(defun sl/friendly-lsp-completion-at-point ()
  "Wraps lsp-completion-at-point in cape-capf-super to make it friendly."
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     'lsp-completion-at-point
                     :with
                     'cape-keyword
                     'cape-abbrev
                     'cape-file))))
(add-hook 'lsp-completion-mode-hook #'sl/friendly-lsp-completion-at-point)

;; LSP booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;;; ----- Flycheck + Flyover -----

(require 'flycheck)
(setq flycheck-emacs-lisp-load-path load-path)

(defun flycheck-eldoc (callback &rest _ignored)
  "Print flycheck messages at point by calling CALLBACK."
  (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
    (mapc
     (lambda (err)
       (funcall callback
                (format "%s: %s"
                        (let ((level (flycheck-error-level err)))
                          (pcase level
                            ('info (propertize "I" 'face 'flycheck-error-list-info))
                            ('error (propertize "E" 'face 'flycheck-error-list-error))
                            ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                            (_ level)))
                        (flycheck-error-message err))
                :thing (or (flycheck-error-id err)
                           (flycheck-error-group err))
                :face 'font-lock-doc-face))
     flycheck-errors)))

(defun flycheck-prefer-eldoc ()
  "Add flycheck-eldoc to eldoc doc functions and disabled default flycheck."
  (add-hook 'eldoc-documentation-functions #'flycheck-eldoc -100 t)
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq flycheck-display-errors-function nil)
  (setq flycheck-help-echo-function nil))
(add-hook 'flycheck-mode-hook 'flycheck-prefer-eldoc)

(defvar flyover-base-height 1)
(require 'flyover)
(setq flyover-error-icon "✘")
(setq flyover-wrap-messages t)
(setq flyover-max-line-length 200)
(set-face-attribute 'flyover-marker nil :foreground "#f1f5f9")
(add-hook 'flycheck-mode-hook 'flyover-mode)

;;; ----- Treesit -----

(require 'treesit)
(setq treesit-font-lock-level 4)
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java . ("https://github.com/tree-sitter/tree-sitter-java"))
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
        (typst . ("https://github.com/uben0/tree-sitter-typst"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
        (c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (nix . ("https://github.com/nix-community/tree-sitter-nix"))
        (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

(require 'treesit-fold)

;;; ----- Languages -----

;; Rust
(require 'lsp-rust)
(setq lsp-rust-analyzer-cargo-target-dir t)
(setq lsp-rust-analyzer-cargo-watch-command "clippy")
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook #'lsp-deferred)

;; Web / Svelte
(require 'web-mode)
(setq web-mode-script-padding 2)
(setq web-mode-code-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
(add-hook 'web-mode-hook #'lsp-deferred)

;; Java
(require 'lsp-java)
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-hook 'java-ts-mode-hook #'lsp-deferred)

;; TypeScript / JavaScript
(require 'typescript-ts-mode)
(require 'js)
(require 'lsp-eslint)
(setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
(add-hook 'typescript-ts-mode-hook #'lsp-deferred)
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)
(add-hook 'js-ts-mode-hook #'lsp-deferred)
(add-hook 'js-jsx-mode-hook #'lsp-deferred)

;; JSON
(require 'json-ts-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;; C / C++
(require 'lsp-clangd)
(add-to-list 'lsp-clients-clangd-args "--log=error")
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

;; SQL
(require 'lsp-sqls)
(setq lsp-sqls-workspace-config-path nil)

(defvar cape-sql-keywords
  '("SERIAL" "PRIMARY KEY" "NOT NULL" "UNIQUE" "DEFAULT"
    "CHECK" "REFERENCES" "INT" "BIGINT" "TEXT" "BOOLEAN"
    "CREATE" "TABLE" "INSERT" "UPDATE" "DELETE" "VALUES"
    "JOIN" "ON DELETE" "ON UPDATE")
  "List of common ANSI SQL keywords for completion.")
(require 'cape-keyword)
(add-to-list 'cape-keyword-list (cons 'sql-mode cape-sql-keywords))

;; Typst
(require 'typst-ts-mode)
(add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))
(lsp-register-client (make-lsp-client
                      :new-connection (lsp-stdio-connection "tinymist")
                      :activation-fn (lsp-activate-on "typst")
                      :server-id 'tinymist))

;;; ----- Tools -----

;; Magit
(require 'magit)
(setq magit-process-apply-ansi-colors t)
(setq transient-default-level 7)

;; Apheleia (formatting)
(require 'apheleia)
(setq apheleia-formatters-respect-indent-level nil)
(add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt"))
(add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixpkgs-fmt))
(add-to-list 'apheleia-mode-alist '(web-mode . prettier-svelte))

;; Yasnippet
(require 'yasnippet)

;; Docstr
(require 'docstr)
(setq docstr-key-support t)

;;; ----- Global Modes -----

(global-flycheck-mode)
(global-treesit-fold-mode)
(global-treesit-fold-indicators-mode)
(apheleia-global-mode t)
(yas-global-mode)
(global-docstr-mode)

;;; dev.el ends here
