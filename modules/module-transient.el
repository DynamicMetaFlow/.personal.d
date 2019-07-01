(use-package transient
  :config
  (add-to-list 'load-path "~/Public/matcha")
  (load-file "~/Public/matcha/matcha.el")
  (matcha-setup))

(require 'matcha-base)
(require 'org)

(define-transient-command org-agenda-process ()
  "Agenda"

  [["Move"
   ("n" "Next" org-next-visible-heading :transient t)
   ("p" "Prev" org-previous-visible-heading :transient t)]

   ["Update"
    ("t" "Todo" org-todo :transient t)
    ("s" "Schedule" org-schedule :transient t)
    ("d" "Deadline" org-deadline :transient t)
    ("h" "Rename" org-rename-header :transient t)
    ("a" "Archive" org-archive-subtree :transient t)
    ("d" "Delete" org-cut-subtree :transient t)
    ("S" "Save.." org-save-all-org-buffers :transient t)]

   ["Create"
    ("P" "Project" org-refile-to-projects-dir :transient t)
    ("N" "Note" org-refile-to-notes-dir :transient t)]

   ["Refile to"
    ("T" "Tasks" org-refile-to-task :transient t)
    ("I" "Incubate" org-refile-to-incubate :transient t)
    ("W" "Waiting" org-refile-to-waiting :transient t)
    ("R" "Other.." org-refile :transient t)]

   ["Calendar"
    ("c c" "Calendar" refile-to-calendar :transient t)
    ("c t" "Tickler" refile-to-tickler :transient t)
    ("c d" "Delegate" org-refile-to-delegate :transient t)]

   ["Reference"
    ("r d" "Document" note-to-documents :transient t)
    ("r w" "Website" note-to-websites :transient t)
    ("r i" "Images" note-to-images :transient t)]])
