;;; utils.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; 
(defun align-string-lines-to-opening-column (&optional extra)
  "Align all subsequent lines of the current string so their first
non-whitespace character starts at the same column as the first
content character after the opening quote. Works with Rust raw strings.

With prefix arg EXTRA (or interactive numeric arg), add EXTRA spaces
to that target column."
  (interactive "P")
  (let* ((ppss (syntax-ppss)))
    (unless (nth 3 ppss)
      (user-error "Not inside a string"))
    (let* ((str-beg (nth 8 ppss))
           (str-end (save-excursion (goto-char str-beg) (forward-sexp) (point)))
           ;; position just after the opening quote (handles r##"...")
           (content-beg (save-excursion
                          (goto-char str-beg)
                          (unless (re-search-forward "\"" str-end t)
                            (user-error "Could not find opening quote"))
                          (point)))
           ;; position of the closing quote
           (content-end (save-excursion
                          (goto-char str-end)
                          (unless (re-search-backward "\"" str-beg t)
                            (user-error "Could not find closing quote"))
                          (point)))
           (extra (prefix-numeric-value (or extra 0)))
           (target-col (save-excursion
                         (goto-char content-beg)
                         (+ (current-column) extra))))
      (save-excursion
        (save-restriction
          (narrow-to-region content-beg content-end)
          ;; Skip the first content line
          (goto-char (point-min))
          (end-of-line)
          ;; Now adjust each subsequent line
          (forward-line 1)
          (while (< (point) (point-max))
            (let ((bol (line-beginning-position))
                  (eol (line-end-position)))
              ;; Ignore completely blank lines
              (if (save-excursion
                    (goto-char bol)
                    (looking-at-p "^[ \t]*$"))
                  nil
                ;; Replace leading whitespace so first non-ws char is at target-col
                (let ((first-non-ws (save-excursion
                                      (goto-char bol)
                                      (skip-chars-forward " \t" eol)
                                      (point))))
                  (let ((current-col (save-excursion
                                       (goto-char first-non-ws)
                                       (current-column))))
                    (when (/= current-col target-col)
                      (let ((spaces (make-string (max 0 (- target-col
                                                           (save-excursion
                                                             (goto-char bol)
                                                             (current-column))))
                                                 ?\s)))
                        ;; Replace from BOL up to first non-ws with the exact spaces needed
                        (delete-region bol first-non-ws)
                        (goto-char bol)
                        (insert spaces)))))))
            (forward-line 1)))))))
;;; utils.el ends here
