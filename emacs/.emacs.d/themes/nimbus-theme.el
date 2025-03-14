;;; nimbus-theme.el --- A soft and tranquil theme with minimal strain -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(deftheme nimbus "A soft and tranquil theme with minimal strain." '(:family 'custom :background-mode 'light))

(require 'color)
(require 'cl-lib)

(defvar nimbus/bg "#ffffff")
(defvar nimbus/fg-default "#1e293b")
(defvar nimbus/fg-neutral "#94a3b8")
(defvar nimbus/fg-subtle "#f1f5f9")
(defvar nimbus/error "#f87171")
(defvar nimbus/warning "#fb923c")
(defvar nimbus/operator "#374151")
(defvar nimbus/keyword "#0891b2")
(defvar nimbus/function "#2563eb")
(defvar nimbus/constant "#7c3aed")

(custom-theme-set-faces 'nimbus
                        `(default ((t . (:background ,nimbus/bg :foreground ,nimbus/fg-default))))
                        `(cursor ((t . (:background ,nimbus/fg-default))))
                        `(region ((t . (:background ,nimbus/fg-subtle))))
                        `(fringe ((t . (:background ,nimbus/bg))))
                        `(mode-line ((t . (:background ,nimbus/fg-subtle :foreground ,nimbus/fg-neutral))))
                        `(font-lock-bracket-face ((t . (:background-color ,nimbus/operator))))
	                    `(font-lock-builtin-face ((t . (:foreground ,nimbus/function))))
	                    `(font-lock-comment-delimiter-face ((t . (:foreground ,nimbus/fg-neutral))))
	                    `(font-lock-comment-face ((t . (:foreground ,nimbus/fg-neutral :slant italic))))
	                    `(font-lock-constant-face ((t . (:foreground ,nimbus/constant))))
	                    `(font-lock-delimiter-face ((t . (:foreground ,nimbus/operator))))
	                    `(font-lock-doc-face ((t . (:foreground ,nimbus/fg-neutral :weight bold :slant italic))))
	                    `(font-lock-doc-markup-face ((t . (:foreground ,nimbus/fg-neutral :slant italic))))
	                    `(font-lock-escape-face ((t . (:foreground ,nimbus/keyword))))
	                    `(font-lock-function-call-face ((t . (:foreground ,nimbus/function))))
	                    `(font-lock-function-name-face ((t . (:foreground ,nimbus/function))))
	                    `(font-lock-keyword-face ((t . (:foreground ,nimbus/keyword))))
	                    `(font-lock-misc-punctuation-face ((t . (:foreground ,nimbus/operator))))
	                    `(font-lock-negation-char-face ((t . (:foreground ,nimbus/keyword))))
	                    `(font-lock-number-face ((t . (:foreground ,nimbus/keyword))))
	                    `(font-lock-operator-face ((t . (:foreground ,nimbus/operator))))
	                    `(font-lock-preprocessor-face ((t . (:foreground ,nimbus/fg-neutral :weight bold))))
	                    `(font-lock-property-name-face ((t . (:foreground ,nimbus/keyword))))
	                    `(font-lock-property-use-face ((t . (:foreground ,nimbus/keyword))))
	                    `(font-lock-punctuation-face ((t . (:foreground ,nimbus/operator))))
	                    `(font-lock-regexp-face ((t . (:foreground ,nimbus/fg-neutral))))
	                    `(font-lock-regexp-grouping-backslash ((t . (:foreground ,nimbus/keyword))))
	                    `(font-lock-regexp-grouping-construct ((t . (:foreground ,nimbus/keyword))))
	                    `(font-lock-string-face ((t . (:foreground ,nimbus/fg-neutral))))
	                    `(font-lock-type-face ((t . (:foreground ,nimbus/constant))))
	                    `(font-lock-variable-name-face ((t . (:foreground ,nimbus/fg-default))))
	                    `(font-lock-variable-use-face  ((t . (:foreground ,nimbus/fg-default))))
	                    `(font-lock-warning-face ((t . (:foreground ,nimbus/warning))))
	                    `(magit-header-line ((t . (:foreground ,nimbus/fg-default))))
	                    `(magit-section-heading ((t . (:foreground ,nimbus/fg-default :weight bold))))
	                    `(magit-diff-file-heading ((t . (:foreground ,nimbus/fg-neutral :weight bold))))
	                    `(magit-tag ((t . (:foreground ,nimbus/fg-default))))
	                    `(magit-head ((t . (:foreground ,nimbus/fg-neutral))))
                        )

(provide-theme 'nimbus)
;;; nimbus-theme.el ends here.
