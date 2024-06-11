;;; latex.el --- -*- lexical-binding: t -*-
;;; commentary:
;; 
;;; code: 

(require 'latex)

;;; Code:

(setq TeX-engine-alist '((default
                          "Tectonic"
                          "tectonic -X compile -f plain %T"
                          "tectonic -X watch"
                          nil)))

(setq LaTeX-command-style '(("" "%(latex)")))
(setq TeX-check-TeX nil)
(setq TeX-process-asynchronous t)
(setq TeX-engine 'default)

(let ((tex-list (assoc "TeX" TeX-command-list))
      (latex-list (assoc "LaTeX" TeX-command-list)))
  (setf (cadr tex-list) "%(tex)"
        (cadr latex-list) "%l"))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when-let ((project (project-current))
                       (proot (project-root project)))
              (when (file-exists-p (expand-file-name "Tectonic.toml" proot))
                (setq-local TeX-output-dir (expand-file-name "build/index" proot))))))

;;; latex.el ends here
