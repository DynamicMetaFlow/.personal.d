(after "org-contrib-autoloads"

(defun my/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the
      current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (my/org-custom-id-get (point) 'create))))

(defun my/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
      If POM is nil, refer to the entry at point. If the entry does
      not have an CUSTOM_ID, the function returns nil. However, when
      CREATE is non nil, create a CUSTOM_ID if none is present
      already. PREFIX will be passed through to `org-id-new'. In any
      case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "EXPORT_FILE_NAME")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "EXPORT_FILENAME" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

   (defun my-org-mode-hooks ()
     (visual-line-mode)
     (turn-on-auto-fill)
     (turn-on-flyspell)
     (outline-minor-mode))

  (defvar my-cpp-other-file-alist
    '(("\\.org\\'" (".org_archive"))
      ))

  (setq-default ff-other-file-alist 'my-cpp-other-file-alist)

  (defun my/insert-created-timestamp()
    "Insert a CREATED property using org-expiry.el for TODO entries"
    (org-entry-put nil "CREATED" (format-time-string "<%Y-%m-%d %a %H:%M>"))
    (org-expiry-insert-created)
    (org-back-to-heading)
    (org-end-of-line)
    (insert " "))

  (add-hook 'org-capture-before-finalize-hook 'my/insert-created-timestamp)
  (add-hook 'org-capture-prepare-finalize-hook 'org-id-store-link)

;   (add-to-list 'org-log-note-headings
;                '(property . "Property %-12s from %-12S %t"))

   (defun my/org-property-change-note (prop val)
     (message (concat "Changing " prop " from \n" val))
     (if (not 'my/org-property-previous-val)
         (if (not (member prop my/org-property-ignored-properties))
         (org-add-log-setup 'property prop my/org-property-previous-val))))


   (defun org-add-log-note (&optional _purpose)
     "Pop up a window for taking a note, and add this note later."
     (remove-hook 'post-command-hook 'org-add-log-note)
     (setq org-log-note-window-configuration (current-window-configuration))
     (delete-other-windows)
     (move-marker org-log-note-return-to (point))
     (pop-to-buffer-same-window (marker-buffer org-log-note-marker))
     (goto-char org-log-note-marker)
     (org-switch-to-buffer-other-window "*Org Note*")
     (erase-buffer)
     (if (memq org-log-note-how '(time state))
         (org-store-log-note)
       (let ((org-inhibit-startup t)) (org-mode))
       (insert (format "# Insert note for %s.
   # Finish with C-c C-c, or cancel with C-c C-k.\n\n"
                       (cond
                        ((eq org-log-note-purpose 'clock-out) "stopped clock")
                        ((eq org-log-note-purpose 'done)  "closed todo item")
                        ((eq org-log-note-purpose 'state)
                         (format "state change from \"%s\" to \"%s\""
                                 (or org-log-note-previous-state "")
                                 (or org-log-note-state "")))
                        ((eq org-log-note-purpose 'reschedule)
                         "rescheduling")
                        ((eq org-log-note-purpose 'delschedule)
                         "no longer scheduled")
                        ((eq org-log-note-purpose 'redeadline)
                         "changing deadline")
                        ((eq org-log-note-purpose 'deldeadline)
                         "removing deadline")
                        ((eq org-log-note-purpose 'refile)
                         "refiling")
                        ((eq org-log-note-purpose 'note)
                         "this entry")
                        ((eq org-log-note-purpose 'property)
                         (format "\"%s\" property change from \"%s\""
                                 (or org-log-note-state "")
                                 (or org-log-note-previous-state "")))
                        (t (error "This should not happen")))))
       (when org-log-note-extra (insert org-log-note-extra))
       (setq-local org-finish-function 'org-store-log-note)
       (run-hooks 'org-log-buffer-setup-hook)))

   (add-hook 'org-property-changed-functions 'my/org-property-change-note)

   (defun my/org-insert-link-all ()
     (interactive)
     (while org-stored-links
       (with-simulated-input "RET RET"
         (org-insert-link))
       (if (last org-stored-links)
           (progn
             (if (org-in-item-p)
                 (org-insert-item)
               (insert ", "))))))


   ;; Record time and note when a task is completed
;   (setq org-log-done 'note)

   ;; Record time and note when the scheduled date of a task is modified
   (setq org-log-reschedule 'note)

   (defun org-archive-done-tasks ()
     (interactive)
     (org-map-entries 'org-archive-subtree "/DONE" 'file))

   (defun org-insert-link-with-default-description (file-name)
     (interactive (list (read-file-name "File: ")))
     (org-insert-link file-name file-name (file-name-nondirectory file-name)))

   (defun my/org-export-headlines-to-org ()
     "Export all subtrees that are *not* tagged with :noexport: to
   separate files.

   Subtrees that do not have the :EXPORT_FILE_NAME: property set
   are exported to a filename derived from the headline text."
     (interactive)
                                           ;  (save-buffer)
     (let ((modifiedp (buffer-modified-p)))
       (save-excursion
         (goto-char (point-min))
         (goto-char (re-search-forward "^*"))
         (set-mark (line-beginning-position))
         (goto-char (point-max))
         (org-map-entries
          (lambda ()
            (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
              (unless export-file
                (org-set-property
                 "EXPORT_FILE_NAME"
                 (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
              (deactivate-mark)
              (org-org-export-to-org nil t)
              (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
              (set-buffer-modified-p modifiedp)))
          "-noexport" 'region-start-level))))

   (defun my/org-capture-task ()
      (interactive)
      (org-capture nil "t"))

    (defun my/org-capture-note ()
      (interactive)
      (org-capture nil "n"))

    (defun my/org-capture-appt ()
      (interactive)
      (org-capture nil "a"))

    (defun my/org-capture-log ()
      (interactive)
      (org-capture nil "l"))

    (defun my/org-capture-journal ()
      (interactive)
      (org-capture nil "j"))

   (defun org-tree-open-in-right-frame ()
     (interactive)
     (org-tree-to-indirect-buffer)
     (windmove-right))

 )
