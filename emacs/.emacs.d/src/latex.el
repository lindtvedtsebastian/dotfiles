;;; latex.el --- -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(require 'latex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(setq TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;;; latex.el ends here
