  (use-package hydra
    :config

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (defun org-subtree-region ()
    "Return a list of the start and end of a subtree."
    (save-excursion
      (list (progn (org-back-to-heading) (point))
            (progn (org-end-of-subtree)  (point)))))

  (defvar org-refile-directly-show-after nil
    "When refiling directly (using the `org-refile-directly'
   function), show the destination buffer afterwards if this is set
   to `t', otherwise, just do everything in the background.")

  (defun org-refile-directly (file-dest)
    "Move the current subtree to the end of FILE-DEST.
   If SHOW-AFTER is non-nil, show the destination window,
   otherwise, this destination buffer is not shown."
    (interactive "fDestination: ")

    (defun dump-it (file contents)
      (find-file-other-window file-dest)
      (goto-char (point-max))
      (insert "\n" contents))

    (save-excursion
      (let* ((region (org-subtree-region))
             (contents (buffer-substring (first region) (second region))))
        (apply 'kill-region region)
        (if org-refile-directly-show-after
            (save-current-buffer (dump-it file-dest contents))
          (save-window-excursion (dump-it file-dest contents))))))

  (defun org-boxes-workflow ()
    "Load the default tasks file and start our hydra on the first task shown."
    (interactive)
    (find-file org-default-inbox-file)
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (org-agenda-process))

  (defun org-refile-to-incubate ()
    "Refile (move) the current Org subtree to `org-default-incubate-file'."
    (interactive)
    (org-todo "HOLD")
    (org-refile-directly org-default-incubate-file)
    (kill-line))

  (defun org-refile-to-waiting ()
    "Refile (move) the current Org subtree to `org-default-incubate-file'."
    (interactive)
    (org-todo "WAITING")

    (org-refile-directly org-default-waiting-file)
    (kill-line))

  (defun org-refile-to-calendar ()
    "Refile (move) the current Org subtree to `org-default-incubate-file'."
    (interactive)
    (org-todo "TODO")

    (org-refile-directly org-default-calendar-file)
    (kill-line))

  (defun org-refile-to-task ()
    "Refile (move) the current Org subtree to `org-default-tasks-file'."
    (interactive)
    (org-todo "TODO")

    (org-refile-directly org-default-tasks-file)
    (kill-line))

  (defun org-refile-to-projects-dir ()
    "Move the current subtree to a file in the `projects' directory."
    (interactive)
    (org-refile-subtree-to-file org-default-projects-dir))

  (defun org-refile-to-technical-dir ()
    "Move the current subtree to a file in the `technical' directory."
    (interactive)
    (org-refile-subtree-to-file org-default-technical-dir))

  (defun org-refile-to-personal-dir ()
    "Move the current subtree to a file in the `personal' directory."
    (interactive)
    (org-refile-subtree-to-file org-default-personal-dir))

  (defun org-refile-to-notes-dir ()
    "Move the current subtree to a file in the `zettelkasten' directory."
    (interactive)
    (org-refile-notes-to-file org-default-notes-dir))

  (defun org-refile-to-headline (file headline)
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (org-refile nil nil (list headline file nil pos))
      (switch-to-buffer (current-buffer))))

  (defun org-refile-to-process ()
    "Refile (move) the current Org subtree to `org-default-incubate-file'."
    (interactive)

    (org-refile-directly "~/org/notes/process.org")
    (kill-line)
    )

  (defun org-refile-to-cross-ref ()
    "Refile (move) the current Org subtree to `org-default-incubate-file'."
    (interactive)

    (org-refile-directly "~/org/notes/cross-reference.org")
    (kill-line))

  (defun note-to-websites ()
    (interactive)
    (org-refile-to-headline org-file-reference "Websites"))

  (defun note-to-images ()
    (interactive)
    (org-refile-to-headline org-file-reference "Images"))

  (defun note-to-videos ()
    (interactive)
    (org-refile-to-headline org-file-reference "Videos"))

  (defun note-to-audio ()
    (interactive)
    (org-refile-to-headline org-file-reference "Audio"))

  (defun note-to-documents ()
    (interactive)
    (org-refile-to-headline org-file-reference "Documents"))

  (defun refile-to-tickler ()
    (interactive)
    (org-refile-to-headline org-file-calendar "Tickler"))

  (defun refile-to-calendar ()
    (interactive)
    (org-refile-to-headline org-file-calendar "Calendar"))

  (defun org-refile-to-delegate ()
    "Refile (move) the current Org subtree to `org-default-incubate-file'."
    (interactive)
    (org-todo "DELEGATE")

    (org-refile-directly org-default-delegate-file)
    (kill-line))

  (defun org-refile-subtree-to-file (dir)
    "Archive the org-mode subtree and create an entry in the
   directory folder specified by DIR. It attempts to move as many of
   the subtree's properties and other features to the new file."
    (interactive "DDestination: ")
    (let* ((props      (org-subtree-metadata))
           (head       (plist-get props :header))
           (body       (plist-get props :body))
           (tags       (plist-get props :tags))
           (properties (plist-get props :properties))
           (area       (plist-get props :region))
           (filename   (org-filename-from-title head))
           (filepath   (format "%s/%s.org" dir filename)))
      (apply #'delete-region area)
      (org-create-org-file filepath head body)))

  (defun org-create-org-file (filepath header body)
    "Create a new Org file by FILEPATH. The contents of the file is
   pre-populated with the HEADER, BODY and any associated TAGS."
    (find-file-other-window filepath)
    (org-set-file-property "TITLE" header t)
    (org-set-file-property "CATEGORY" "project")
  ;  (when tags
  ;    (org-set-file-property "FILETAGS" (s-join " " tags)))

    ;;  Insert any drawer properties as #+PROPERTY entries:
    (when properties
      (goto-char (point-min))
      (or (re-search-forward "^\s*_\s$" nil t) (point-max))
      (--map (insert (format "#+PROPERTY: %s %s\n" (first it) (second it))) properties))

    ;; My auto-insert often adds an initial headline for a subtree, and in this
    ;; case, I don't want that... Yeah, this isn't really globally applicable,
    ;; but it shouldn't cause a problem for others.
    (when (re-search-forward "^\\* [0-9]$" nil t)
      (replace-match ""))

    (delete-blank-lines)
    (goto-char (point-max))
    (insert "\n")
    (insert "\n")
    (insert "* TODO " header)
    (insert "\n")
    (insert body))

  (defun org-refile-notes-to-file (dir)
    "Archive the org-mode subtree and create an entry in the
    directory folder specified by DIR. It attempts to move as many of
    the subtree's properties and other features to the new file."
    (interactive "DDestination: ")
    (let* ((props      (org-subtree-metadata))
           (head       (plist-get props :header))
           (body       (plist-get props :body))
           (tags       (plist-get props :tags))
                                          ;          (properties (plist-get props :properties))
           (area       (plist-get props :region))
           (filename   (concat (format-time-string "%Y-%m-%dT%H.%M.%S")))
           (filepath   (format "%s/%s.org" dir filename)))
      (apply #'delete-region area)
      (org-create-notes-file filepath head body tags)))

  (defun org-create-notes-file (filepath header body tags)
    "Create a new Org file by FILEPATH. The contents of the file is
    pre-populated with the HEADER, BODY and any associated TAGS."
    (find-file filepath)
    (org-set-file-property "TITLE" header t)
    (org-set-file-property "DATE" (format-time-string "<%Y-%m-%d %H:%M>"))
    (org-set-file-property "KEYWORDS" (s-join "" tags))
    (goto-char (point-min))
    (when (re-search-forward "REFILE" nil t)
      (replace-match ""))

    (delete-blank-lines)
    (goto-char (point-max))
    (insert "\n\n")
                                          ;   (insert "* " header)
                                          ;   (insert "\n\n")
    (insert body)
    (goto-char (point-min))
    (save-buffer))

  (defun org-rename-header (label)
    "Rename the current section's header to LABEL, and moves the
    point to the end of the line."
    (interactive (list
                  (read-string "Header: "
                               (substring-no-properties (org-get-heading t t t t)))))
    (org-back-to-heading)
    (replace-string (org-get-heading t t t t) label))

  (defun todays-journal-entry ()
    "Return the full pathname to the day's journal entry file.
    Granted, this assumes each journal's file entry to be formatted
    with year/month/day, as in `20190104' for January 4th.

    Note: `org-journal-dir' variable must be set to the directory
    where all good journal entries live, e.g. ~/journal."
    (let* ((daily-name   (format-time-string "%Y-%m-%d"))
           (file-name    (concat org-journal-dir daily-name)))
      (expand-file-name file-name)))

(cl-defun unpackaged/org-refile-to-datetree (&key (date (calendar-current-date)) entry)
  "Refile ENTRY or current node to entry for DATE in datetree in FILE."
  (interactive)
  (require 'org-datetree)
  (setq file "~/org/agenda/log.org")
  (unless entry
    (org-cut-subtree))
  (condition-case err
      (with-current-buffer (or (org-find-base-buffer-visiting file)
                               (find-file-noselect file))
        (org-datetree-file-entry-under (or entry (car kill-ring)) date)
        (save-buffer))
    (error (unless entry
             (org-paste-subtree))
           (message "Unable to refile! %s" err))))


  (defun org-subtree-metadata ()
    "Return a list of key aspects of an org-subtree. Includes the
    following: header text, body contents, list of tags, region list
    of the start and end of the subtree."
    (save-excursion
      ;; Jump to the parent header if not already on a header
      (when (not (org-at-heading-p))
        (org-previous-visible-heading 1))

      (let* ((context (org-element-context))
             (attrs   (second context))
             (props   (org-entry-properties)))

        (list :region     (list (plist-get attrs :begin) (plist-get attrs :end))
              :header     (plist-get attrs :title)
              :tags       (org-get-subtree-tags props)
              :properties (org-get-subtree-properties attrs)
              :body       (org-get-subtree-content attrs)))))

  (defun org-get-subtree-tags (&optional props)
    "Given the properties, PROPS, from a call to
    `org-entry-properties', return a list of tags."
    (unless props
      (setq props (org-entry-properties)))
    (let ((tag-label (if org-get-subtree-tags-inherited "ALLTAGS" "TAGS")))
      (-some->> props
                (assoc tag-label)
                cdr
                substring-no-properties
                (s-split ":")
                (--filter (not (equalp "" it))))))

  (defvar org-get-subtree-tags-inherited t
    "Returns a subtree's tags, and all tags inherited (from tags
      specified in parents headlines or on the file itself). Defaults
      to true.")

  (defun org-get-subtree-properties (attributes)
    "Return a list of tuples of a subtrees properties where the keys are strings."

    (defun symbol-upcase? (sym)
      (let ((case-fold-search nil))
        (string-match-p "^:[A-Z]+$" (symbol-name sym))))

    (defun convert-tuple (tup)
      (let ((key (first tup))
            (val (second tup)))
        (list (substring (symbol-name key) 1) val)))

    (->> attributes
         (-partition 2)                         ; Convert plist to list of tuples
         (--filter (symbol-upcase? (first it))) ; Remove lowercase tuples
         (-map 'convert-tuple)))

  (defun org-get-subtree-content (attributes)
    "Return the contents of the current subtree as a string."
    (let ((header-components '(clock diary-sexp drawer headline inlinetask
                                     node-property planning property-drawer section)))

      (goto-char (plist-get attributes :contents-begin))

      ;; Walk down past the properties, etc.
      (while
          (let* ((cntx (org-element-context))
                 (elem (first cntx))
                 (props (second cntx)))
            (when (member elem header-components)
              (goto-char (plist-get props :end)))))

      ;; At this point, we are at the beginning of what we consider
      ;; the contents of the subtree, so we can return part of the buffer:
      (buffer-substring-no-properties (point) (org-end-of-subtree))))

  (defun org-filename-from-title (title)
    "Creates a useful filename based on a header string, TITLE.
    For instance, given the string:    What's all this then?
         This function will return:    whats-all-this-then"
    (let* ((no-letters (rx (one-or-more (not alphanumeric))))
           (init-try (->> title
                          downcase
                          (replace-regexp-in-string "'" "")
                          (replace-regexp-in-string no-letters "-"))))
      (string-trim init-try "-+" "-+")))

  (defun org-set-file-property (key value &optional spot)
    "Make sure file contains a top-level, file-wide property.
    KEY is something like `TITLE' or `FILETAGS'. This function makes
    sure that the property contains the contents of VALUE, and if the
    file doesn't have the property, it is inserted at either SPOT, or
    if nil,the top of the file."
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (if (re-search-forward (format "^#\\+%s:\s*\\(.*\\)" key) nil t)
            (replace-match value nil nil nil 1)

          (cond
           ;; if SPOT is a number, go to it:
           ((numberp spot) (goto-char spot))
           ;; If SPOT is not given, jump to first blank line:
           ((null spot) (progn (goto-char (point-min))
                               (re-search-forward "^\s*$" nil t)))
           (t (goto-char (point-min))))

          (insert (format "#+%s: %s\n" (upcase key) value))))))

)
