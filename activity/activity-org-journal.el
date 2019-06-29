      (use-package org-journal
        :config

      (setq journal-author "Alexander Soto")

      ;; This is the base folder where all your "books"
      ;; will be stored.
      (setq journal-base-dir "~/org/notes")


      ;; These are your "books" (folders), add as many as you like.
      ;; Note: "sub volumes" are acheivable with sub folders.
      (setq journal-books
            '("projects"
              "personal"))

      ;; Functions for journal
      (defun get-journal-file-today (book)
        "Return today's filename for a books journal file."
        (interactive (list (completing-read "Book: " journal-books) ))
        (cond

         ((string-match "personal" book)
          (expand-file-name
           (concat journal-base-dir book "/"
                   (format-time-string "%Y-%m-%dT%H.%M.%S") ".org.gpg" )))

         ((unless (equal book '("personal"))
            (expand-file-name
             (concat journal-base-dir book "/"
                     (format-time-string "%Y-%m-%dT%H.%M.%S") ".org" ))))))

      (defun journal-today ()
        "Load todays journal entry for book"
        (interactive)
        (auto-insert-mode)
        (find-file (call-interactively 'get-journal-file-today)) )

      (defun journal-entry-date ()
        "Inserts the journal heading based on the file's name."
        (when (string-match
               "\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\(.org\\)"
               (buffer-name))
          (let ((year  (string-to-number (match-string 2 (buffer-name))))
                (month (string-to-number (match-string 3 (buffer-name))))
                (day   (string-to-number (match-string 4 (buffer-name))))
                (datim nil))
            (setq datim (encode-time 0 0 0 day month year))
            (format-time-string "%Y-%m-%d (%A)" datim))))

            (eval-after-load 'autoinsert
        '(define-auto-insert
           '("\\(20[0-9][0-9]\\)\\(-\\)\\([0-9][0-9]\\)\\(-\\)\\([0-9][0-9]\\)\\(-\\)\\([0-9][0-9][0-9][0-9][0-9][0-9]\\)\\(.*\\)" . "Journal Header")
           '("Short description: "
             "#+TITLE: "
             (read-string "Title: ") \n
             "#+DATE: " (format-time-string "<%Y-%m-%d %H:%M>") \n
             "#+KEYWORDS: "
             (read-string "Keyword: ") \n \n

             > _ \n \n \n \n \n
             "
      ---
      *Related:*


      ---
      *References:*
      "

             )))

)
