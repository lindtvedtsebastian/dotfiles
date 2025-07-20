;;; dev.el --- -*- lexical-binding: t -*-
;;; commentary:
;; dev Emacs configurations
;;; code:

(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(require 'lsp-mode)
(require 'lsp-completion)
(require 'lsp-icons)
(require 'lsp-headerline)

(setq lsp-keymap-prefix "C-c l")
(setq lsp-completion-provider :none)
(setq lsp-log-io nil)
(setq lsp-keep-workspace-alive nil)
(setq lsp-idle-delay 0)
(setq lsp-enable-xref t)
(setq lsp-enable-imenu t)
(setq lsp-enable-file-watchers t)
(setq lsp-file-watch-threshold 10000)
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

(require 'lsp-completion)
(defun corfu-lsp-setup ()
  "Configure lsp-mode for proper corfu/orderless use."
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-completion-mode-hook #'corfu-lsp-setup)

(require 'flycheck)
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
  (add-hook 'eldoc-documentation-functions #'flycheck-eldoc nil t)
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq flycheck-display-errors-function nil)
  (setq flycheck-help-echo-function nil))

(add-hook 'flycheck-mode-hook 'flycheck-prefer-eldoc)

(require 'lsp-rust)
(when (eq system-type 'darwin)
  (setq lsp-rust-analyzer-cargo-extra-env ["MACOSX_DEPLOYMENT_TARGET=10.13"])
  (setq lsp-rust-target-dir "target/analyzer"))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook #'lsp)

(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
(add-hook 'web-mode-hook #'lsp)

;; Magit
(require 'magit)
(setq magit-process-finish-apply-ansi-colors t)   ; Apply colors in Magit process
(setq transient-default-level 6)                  ; Show more transient commands

;; Treesit
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
        (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

(add-to-list 'load-path "~/dotfiles/dependencies/qml-ts-mode")
(require 'qml-ts-mode)
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-ts-mode))

(require 'apheleia)
(apheleia-global-mode t)
(setq apheleia-formatters-respect-indent-level nil)

(add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt"))
(add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixpkgs-fmt))

(require 'web-mode)
(add-to-list 'apheleia-mode-alist '(web-mode . prettier-svelte))
(setq web-mode-script-padding 2)
(setq web-mode-code-indent-offset 2)
(add-hook 'web-mode-hook #'lsp)

(require 'typescript-ts-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-hook 'typescript-ts-mode-hook #'lsp)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-hook 'tsx-ts-mode-hook #'lsp)

(require 'lsp-eslint)
(setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))

(require 'json-ts-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

(require 'flycheck)
(setq flycheck-emacs-lisp-load-path load-path)
(global-flycheck-mode)

(require 'yasnippet)
(yas-global-mode)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

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
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
;;; dev.el ends here
