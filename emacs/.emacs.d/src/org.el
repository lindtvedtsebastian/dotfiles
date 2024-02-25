;;; org.el --- -*- lexical-binding: t -*-
;;; commentary:
;; Emacs configurations for org mode
;;; code:

(require 'org)
(setq org-directory "~/st/org")

(require 'org-roam)

(setq org-roam-directory "~/st/org/roam")
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

(defun sl/org-roam-list-notes-by-tag (tag-name)
  "Iterate over all org-roam nodes, return a list of all nodes containing TAG-NAME."
  (mapcar #'org-roam-node-file
          (seq-filter
           (sl/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun sl/org-roam-refresh-project-agenda-list ()
  "Update the agenda files with all org-roam notes that has a project tag."
  (interactive)
  (setq org-agenda-files (sl/org-roam-list-notes-by-tag "project")))

;; Refresh the agenda list the first time this file loads
(sl/org-roam-refresh-project-agenda-list)

(org-roam-db-autosync-mode)

;;; org.el ends here
