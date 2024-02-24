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
                                  ("t" "todo" plain "* TODO %?" :if-new (file+head "inbox.org" "#+title: inbox\n"))
                                  )))

(org-roam-db-autosync-mode)

;;; org.el ends here
