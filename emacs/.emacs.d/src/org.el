;;; org.el --- -*- lexical-binding: t -*-
;;; commentary:
;; Emacs configurations for org mode
;;; code:

(require 'org)
(setq org-directory "~/db/org")
(setq org-log-done 'time)

(require 'org-refile)
(setq org-refile-targets '((org-agenda-files . (:level . 1))))

(require 'org-roam)

(setq org-roam-directory "~/db/org/roam")
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:30}" 'face 'org-tag)))

(defun sl/org-roam-capture-inbox ()
  "Capture something directly into the roam inbox."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("n" "note" plain "* %?"
                                   :if-new (file+head "inbox.org" "#+title: inbox\n"))
                                  ("t" "todo" plain "* TODO %?" :if-new (file+head "inbox.org" "#+title: inbox\n")))))

(defun sl/org-roam-filter-by-tag (tag-name)
  "Return a function that check if NODE has the tag TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun sl/org-roam-filter-by-tags (tag-list)
  "Return a function that check if NODE has all TAG-LIST tags."
  (lambda (node)
    (let ((tags (org-roam-node-tags node)))
      (seq-every-p (lambda (tag) (member tag tags)) tag-list))))

(defun sl/org-roam-list-notes-by-tag (tag-name)
  "Iterate over all org-roam nodes, return a list of all nodes containing TAG-NAME."
  (mapcar #'org-roam-node-file
          (seq-filter
           (sl/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun sl/org-roam-list-active-projects ()
  "Return list of files with project and active tags."
  (mapcar #'org-roam-node-file
          (seq-filter
           (sl/org-roam-filter-by-tags '("project" "active"))
           (org-roam-node-list))))

(defun sl/org-roam-refresh-agenda-list ()
  "Update the agenda files with only active projects and recent dailies."
  (interactive)
  (let* ((dailies-dir (expand-file-name "daily" org-roam-directory))
         (recent-dailies (seq-filter (lambda (f)
                                       (let ((days-back 10)
                                             (file-date (file-name-base f)))
                                         (and (string-match "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" file-date)
                                              (let* ((year (string-to-number (match-string 1 file-date)))
                                                     (month (string-to-number (match-string 2 file-date)))
                                                     (day (string-to-number (match-string 3 file-date)))
                                                     (file-time (encode-time 0 0 0 day month year)))
                                                (<= (abs (time-to-days (time-subtract (current-time) file-time))) days-back)))))
                                     (directory-files dailies-dir t "\\.org$")))
         (projects (sl/org-roam-list-active-projects)))
    (setq org-agenda-files (append recent-dailies projects))))

(defun sl/org-roam-project-finalize-hook ()
  "Update variable `org-agenda-files' with project if the capture was not aborted."
  (remove-hook 'org-capture-after-finalize-hook #'sl/org-roam-project-finalize-hook)
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun sl/org-roam-find-project ()
  "Find or create a new org-roam project."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'sl/org-roam-project-finalize-hook)
  (org-roam-node-find
   nil
   nil
   (sl/org-roam-filter-by-tags '("project"))
   nil
   :templates
   (sl/load-capture-template "project")))

(defun sl/toggle-project-active ()
  "Toggle the :active: tag on the current project note."
  (interactive)
  (let ((tags (org-get-tags)))
    (if (member "active" tags)
        (progn
          (org-toggle-tag "active" 'off)
          (message "Project marked as INACTIVE."))
      (org-toggle-tag "active" 'on)
      (message "Project marked as ACTIVE.")))
  (sl/org-roam-refresh-agenda-list))

(defun sl/load-capture-template (template-name)
  "Load capture template from ~/.emacs.d/templates/TEMPLATE-NAME.org."
  (let ((template-file (expand-file-name (concat template-name ".org") "~/.emacs.d/templates/")))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        (buffer-string)))))

(sl/org-roam-refresh-agenda-list)  ; Refresh the agenda list the first time this file loads
(org-roam-db-autosync-mode)        ; Automatically sync org-roam db on changes

;;; org.el ends here
