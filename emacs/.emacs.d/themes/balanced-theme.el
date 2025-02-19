;;; balanced-theme.el --- A balanced and subtle theme -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(deftheme balanced "A balanced and subtle theme." '(:family 'custom :background-mode 'light))

(require 'color)
(require 'cl-lib)

(defvar balanced/primary "#b75c3a")
;; (defvar balanced/primary "#fcf857")
(defvar balanced/bg "#ffffff")
(defvar balanced/fg-subtle "#c1cdd1")
(defvar balanced/fg-neutral "#616e75")
(defvar balanced/fg-vibrant "#020202")
(defvar balanced/error "#cc3333")
(defvar balanced/warning "#eb9947")
(defvar balanced/operator "#737373")
(defvar balanced/selection (cl-destructuring-bind
                               (h _ _) (apply 'color-rgb-to-hsl (color-name-to-rgb balanced/primary))
                             (let ((hue (mod (+ h (/ 20.0 360.0)) 1.0)))
                               (apply 'color-rgb-to-hex (color-hsl-to-rgb hue .10 .90)))))
(defvar balanced/comment (cl-destructuring-bind
                             (h _ _) (apply 'color-rgb-to-hsl (color-name-to-rgb balanced/primary))
                           (let ((hue (mod (+ h (/ 160.0 360.0)) 1.0)))
                             (apply 'color-rgb-to-hex (color-hsl-to-rgb hue .20 .40)))))
(defvar balanced/string (cl-destructuring-bind
                            (h _ _) (apply 'color-rgb-to-hsl (color-name-to-rgb balanced/primary))
                          (let ((hue (mod (+ h (/ 160.0 360.0)) 1.0)))
                            (apply 'color-rgb-to-hex (color-hsl-to-rgb hue .40 .50)))))
(defvar balanced/keyword (cl-destructuring-bind
                             (h _ _) (apply 'color-rgb-to-hsl (color-name-to-rgb balanced/primary))
                           (let ((hue (mod (+ h (/ 30.0 360.0)) 1.0)))
                             (apply 'color-rgb-to-hex (color-hsl-to-rgb hue .35 .35)))))
(defvar balanced/function (cl-destructuring-bind
                              (h _ _) (apply 'color-rgb-to-hsl (color-name-to-rgb balanced/primary))
                            (let ((hue (mod (+ h (/ 250.0 360.0)) 1.0)))
                              (apply 'color-rgb-to-hex (color-hsl-to-rgb hue .50 .55)))))
(defvar balanced/constant (cl-destructuring-bind
                              (h _ _) (apply 'color-rgb-to-hsl (color-name-to-rgb balanced/primary))
                            (let ((hue (mod (+ h (/ 45.0 360.0)) 1.0)))
                              (apply 'color-rgb-to-hex (color-hsl-to-rgb hue .70 .60)))))

(custom-theme-set-faces 'balanced
                        `(default ((t . (:background ,balanced/bg :foreground ,balanced/fg-neutral))))
                        `(cursor ((t . (:background ,balanced/fg-vibrant))))
                        `(region ((t . (:background ,balanced/selection))))
                        `(fringe ((t . (:background ,balanced/bg))))
                        `(mode-line ((t . (:background ,balanced/fg-subtle :foreground ,balanced/fg-neutral))))
                        `(font-lock-bracket-face ((t . (:background-color ,balanced/operator))))
	                    `(font-lock-builtin-face ((t. (:foreground ,balanced/function))))
	                    `(font-lock-comment-delimiter-face ((t . (:foreground ,balanced/string))))
	                    `(font-lock-comment-face ((t . (:foreground ,balanced/comment))))
	                    `(font-lock-constant-face ((t . (:foreground ,balanced/constant))))
	                    `(font-lock-delimiter-face ((t . (:foreground ,balanced/operator))))
	                    `(font-lock-doc-face ((t . (:foreground ,balanced/comment))))
	                    `(font-lock-doc-markup-face ((t . (:foreground ,balanced/string))))
	                    `(font-lock-escape-face ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-function-call-face ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-function-name-face ((t . (:foreground ,balanced/function))))
	                    `(font-lock-keyword-face ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-misc-punctuation-face ((t . (:foreground ,balanced/operator))))
	                    `(font-lock-negation-char-face ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-number-face ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-operator-face ((t . (:foreground ,balanced/operator))))
	                    `(font-lock-preprocessor-face ((t . (:foreground ,balanced/comment))))
	                    `(font-lock-property-name-face ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-property-use-face ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-punctuation-face ((t . (:foreground ,balanced/operator))))
	                    `(font-lock-regexp-face ((t . (:foreground ,balanced/string))))
	                    `(font-lock-regexp-grouping-backslash ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-regexp-grouping-construct ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-string-face ((t . (:foreground ,balanced/string))))
	                    `(font-lock-type-face ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-variable-name-face ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-variable-use-face  ((t . (:foreground ,balanced/keyword))))
	                    `(font-lock-warning-face ((t . (:foreground ,balanced/warning))))
                        )

(provide-theme 'balanced)
;;; balanced-theme.el ends here.
