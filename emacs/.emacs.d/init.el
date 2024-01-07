;;; init.el --- -*- lexical-binding: t -*-
;;; commentary:
;;; initialization file for Emacs
;;; code:

(defvar sl/load-times `(("emacs" . ,(current-time))) "Different timestamps to be collected during initialization.")

(defvar straight-repository-branch "develop")
(defvar straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; List of packages to install
(defvar package-list
      '(orderless            ; Completion style for regexp in any order
        vertico              ; VERTical Interactive COmpletion
        marginalia           ; Completion annotations
        consult              ; Consulting completion-read
        embark               ; Act on minibuffer completions
        embark-consult       ; Consult integration for embark
        corfu                ; Completion Overlay Region FUnction
        cape                 ; Completion at point extensions
        kind-icon            ; Completion kind icons
        magit                ; Git client
        which-key            ; Keybinding information
        exec-path-from-shell ; Env variables from the shell
        ligature             ; Ligatures
        diminish             ; Ability to diminish minor modes in modeline
        yasnippet            ; Yet Another Snippet library
        evil                 ; Extensible VI Layer
        evil-collection      ; Collection of Evil bindings for parts of Emacs not covered by evil
        evil-nerd-commenter  ; Comment/uncomment lines efficiently
        org-roam             ; Plain-text personal knowledge management system
        org-roam-ui          ; UI for org-roam
        scroll-on-drag       ; Interactive scrolling
	    apheleia             ; Formatting all the things
        tree-sitter-langs    ; Grammar bundle for tree-sitter
        graphql-mode         ; Major mode for graphql files
        swift-mode))         ; Major mode for swift files
        
(require 'straight) ; Make flymake happy

;; Install all packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

;; Install gdscript-mode from repository
(straight-use-package '(gdscript-mode :type git :host github :repo "godotengine/emacs-gdscript-mode"))

;; Function for loading directories recursively
(defun load-directory (directory)
  "Recursively load all .el files in a DIRECTORY."
  (dolist (file (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car file))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr file)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (file-name-extension path) "el"))
        (add-to-list 'sl/load-times `(,(file-name-base fullpath) . ,(current-time)))
        (load (file-name-sans-extension fullpath)))))))

;; Load all .el files from ~/.emacs.d/src recursively
(load-directory "~/.emacs.d/src")

;; Report startup times
(let ((total 0)
      (prev (current-time)))
  (message "\n%-20s %s\n" "SECTION" "TIME")
  (dolist (section sl/load-times)
    (message "%-20s %.2fs"
             (car section)
             (float-time (time-subtract prev (cdr section))))
    (setq total (+ total (float-time (time-subtract prev (cdr section)))))
    (setq prev (cdr section)))
  (message "\n%-20s %.2fs\n" "total" total))
    
;;; init.el ends here
