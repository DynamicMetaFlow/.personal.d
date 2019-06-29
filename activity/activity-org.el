(after "org-contrib-autoloads"

  ;; Keep tasks with dates on the global todo lists
  (setq org-agenda-todo-ignore-with-date nil)

  ;; Keep tasks with deadlines on the global todo lists
  (setq org-agenda-todo-ignore-deadlines nil)

  ;; Keep tasks with scheduled dates on the global todo lists
  (setq org-agenda-todo-ignore-scheduled nil)

  ;; Keep tasks with timestamps on the global todo lists
  (setq org-agenda-todo-ignore-timestamp nil)

  ;; Remove completed deadline tasks from the agenda view
  (setq org-agenda-skip-deadline-if-done nil)

  ;; Remove completed scheduled tasks from the agenda view
  (setq org-agenda-skip-scheduled-if-done nil)

  ;; Remove completed items from search results
  (setq org-agenda-skip-timestamp-if-done nil)

  ;; Skip scheduled items if they are repeated beyond the current deadline.
  (setq org-agenda-skip-scheduled-if-deadline-is-shown  (quote repeated-after-deadline))

  (setq org-agenda-include-diary nil)
  (setq org-agenda-insert-diary-extract-time t)

  (setq org-default-notes-file "~/org/notes/inbox.org")

  ;; =TODO= state keywords and colour settings:
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w)" "HOLD(h)" "DELEGATE(D)" "|" "CANCELLED(c@/!)" "MEETING" "SCHEDULED")
                (sequence "ACTIVE(a)" "|" "DISABLED(i)")
                )))

  ;; ;; TODO Other todo keywords doesn't have appropriate faces yet. They should
  ;; ;; have faces similar to spacemacs defaults.
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "#007cee" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("DELEGATE" :foreground "purple" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "red" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("SCHEDULED" :foreground "forest green" :weight bold))))

  (setq org-use-fast-todo-selection t)

  ;; This cycles through the todo states but skips setting timestamps and
  ;; entering notes which is very convenient when all you want to do is fix
  ;; up the status of an entry.
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)
                                   ("~/.personal.d/personal-config.org" :maxlevel . 9)
                                   ("~/.emacs.d/config.org" :maxlevel . 9)
                                   )))

  (setq org-refile-use-outline-path 'file
        org-indent-indentation-per-level nil
        org-outline-path-complete-in-steps nil)


  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  (setq org-refile-target-verify-function 'bh/verify-refile-target)

  (setq org-agenda-clock-consistency-checks
        (quote (:max-duration "4:00"
                              :min-duration 0
                              :max-gap 0
                              :gap-ok-around ("4:00"))))

  ;; Agenda clock report parameters
  (setq org-agenda-clockreport-parameter-plist
        (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
  ;; Separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)

  (setq org-log-into-drawer t)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; Enable auto clock resolution for finding open clocks
                                          ;(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  (setq org-clock-auto-clock-resolution nil)
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)
  ;; Resolve open clocks if the user is idle for more than 10 minutes.
  (setq org-clock-idle-time 10)
  ;;
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

  (setq bh/keep-clock-running nil)


  (setq org-time-stamp-rounding-minutes (quote (1 1)))
  ;; ;; Sometimes I change tasks I'm clocking quickly - this removes clocked
  ;; ;; tasks with 0:00 duration
  ;; (setq org-clock-out-remove-zero-time-clocks t)

  ;; Set default column view headings: Task Effort Clock_Summary
  (setq org-columns-default-format
        "%1PRIORITY %50ITEM %13AREA %10DEADLINE %50OUTCOME %10Effort(Effort){:} %10CLOCKSUM")
  ;; global Effort estimate values
  ;; global STYLE property values for completion
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit"))))

  (setq org-fast-tag-selection-include-todo t)

  (setq org-tag-alist (quote (("WAITING" . ?w)
                              ("HOLD" . ?h)
                              ("NOTE" . ?n)
                              ("CANCELLED" . ?c)
                              ("FLAGGED" . ??))))

  (setq org-agenda-hide-tags-regexp "noexport\\|HOLD\\|REFILE\\|ARCHIVE\\|NOW\\|ignore\\|WAITING\\|nobrain\\|connect\\|clarify\\|capture\\|ATTACH")

  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))

    (require 'org-expiry)

    (setq org-expiry-created-property-name "CREATED")

    (setq org-expiry-inactive-timestamps t)
    (org-expiry-insinuate)

    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window)

  (setq org-modules '(org-habit
                      org-timer
                      org-collector))

  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (scheme . t)
           (dot . t)
           (ditaa . t)
           (python . t)
           (gnuplot . t)
           (shell . t)
           (ledger . t)
           (org . t)
           (plantuml . t)
           (latex . t))))

  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it))



  (defun bh/display-inline-images ()
    (condition-case nil
        (org-display-inline-images)
      (error nil)))

  (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)



  (setq org-confirm-babel-evaluate nil)

  (setq org-babel-results-keyword "results")

  (setq org-ditaa-jar-path "~/.emacs.d/ditaa0_9/ditaa0_9.jar")

(require 'org-habit)

  (setq org-habit-show-habits-only-for-today t)
  (setq org-agenda-tags-column -100
        org-habit-graph-column 45
        org-habit-preceding-days 28
        org-agenda-start-with-log-mode nil)

  (setq org-habit-show-habits nil)

  (setq org-agenda-entry-text-maxlines 10)

  (setq org-agenda-entry-text-leaders " ")

  (setq org-agenda-inhibit-startup t)

  (setq org-agenda-use-tag-inheritance nil)

  (setq org-agenda-files (apply 'append
                                (mapcar
                                 (lambda (directory)
                                   (directory-files-recursively
                                    directory org-agenda-file-regexp))
                                 '("~/org/projects/" "~/org/agenda/" ))))

  (setq org-agenda-timegrid-use-ampm t)

  (setq org-agenda-use-time-grid nil)


  (setq org-show-entry-below (quote ((default))))

  ;; Tags with fast selection keys
  ;; Disable the default org-mode stuck projects agenda view
  (setq org-stuck-projects (quote ("" nil nil "")))

  ;; Limit restriction lock highlighting to the headline only
  (setq org-agenda-restriction-lock-highlight-subtree nil)

  ;; Always hilight the current agenda line
  (add-hook 'org-agenda-mode-hook
            '(lambda () (hl-line-mode 1))
            'append)



  (setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)


  ;; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)



  (setq org-agenda-inhibit-startup t)

  (setq org-agenda-span 'day)

  (setq org-agenda-follow-indirect t)

  (defvar org-timeline-files nil
    "The files to be included in `org-timeline-all-files'. Follows
          the same rules as `org-agenda-files'")

  (setq org-timeline-files '("~/org/agenda/archive"))


  ;; Overwrite the current window with the agenda
  (setq org-agenda-window-setup 'current-window)

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)

  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)

  ;; Agenda clock report parameters
  (setq org-agenda-clockreport-parameter-plist
        (quote (:link t :maxlevel 5 :fileskip0 t :compact nil :narrow 80)))

  ;; Agenda log mode items to display (closed and state changes by default)
  (setq org-agenda-log-mode-items (quote (closed state clock)))

  ;; Keep tasks with dates on the global todo lists
  (setq org-agenda-todo-ignore-with-date nil)

  ;; Keep tasks with deadlines on the global todo lists
  (setq org-agenda-todo-ignore-deadlines nil)

  ;; Keep tasks with scheduled dates on the global todo lists
  (setq org-agenda-todo-ignore-scheduled nil)

  ;; Keep tasks with timestamps on the global todo lists
  (setq org-agenda-todo-ignore-timestamp nil)

  ;; Remove completed deadline tasks from the agenda view
  (setq org-agenda-skip-deadline-if-done t)

  ;; Remove completed scheduled tasks from the agenda view
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Remove completed items from search results
  (setq org-agenda-skip-timestamp-if-done t)

  ;; Include agenda archive files when searching for things
  (setq org-agenda-text-search-extra-files (quote (agenda-archives)))

  ;; Show all future entries for repeating tasks
  (setq org-agenda-repeating-timestamp-show-all t)

  ;; Show all agenda dates - even if they are empty
  (setq org-agenda-show-all-dates t)

  ;; Start the weekly agenda on Monday
  (setq org-agenda-start-on-weekday 1)

  (setq org-default-priority ?C)

(defun my/org-agenda ()
(interactive)
(org-agenda nil " "))


  ;; Use sticky agenda's so they persist
  ;;(setq org-agenda-sticky t)

  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
        (quote (("n" "Notes" tags "note"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                ("h" "Habits" tags-todo "STYLE=\"habit\""
                 ((org-agenda-overriding-header "Habits")
                  (org-agenda-sorting-strategy
                   '(todo-state-down effort-up category-keep))))
                (" " "Agenda"
                 ((agenda "" nil)
                  (tags-todo "-DONE+REFILE"
                             ((org-agenda-overriding-header "Tasks to Refile")
                              (org-tags-match-list-sublevels nil)))

  (tags-todo "-CANCELLED/!"
                             ((org-agenda-overriding-header "Stuck Projects")
                              (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                              (org-agenda-sorting-strategy
                               '(category-keep))))

  (tags-todo "-HOLD-CANCELLED/!"
                             ((org-agenda-overriding-header "Projects")
                              (org-agenda-skip-function 'bh/skip-non-projects)
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy
                               '(priority-down
                                 category-keep))))

  (tags-todo "-CANCELLED/!NEXT"
                             ((org-agenda-overriding-header (concat "Next Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                              (org-tags-match-list-sublevels t)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(priority-down effort-up category-keep))))

                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Subtasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))


  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Actions"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(todo-state-down
                                 category-keep))))

                  (tags-todo "-CANCELLED+WAITING|HOLD/!"
                             ((org-agenda-overriding-header (concat "Waiting and Postponed"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-tasks)
                              (org-tags-match-list-sublevels nil)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(todo-state-down
                                 category-keep))))) nil)

                ("c" "Completed" ((tags "-REFILE/+DONE"
                                        ((org-agenda-overriding-header "Completed Tasks")
                                         (org-tags-match-list-sublevels nil)))))

                )))
  ;; Limit restriction lock highlighting to the headline only
  (setq org-agenda-restriction-lock-highlight-subtree nil)


  ;; Sorting order for tasks on the agenda
  (setq org-agenda-sorting-strategy
        (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
                (todo category-up effort-up)
                (tags category-up effort-up)
                (search category-up))))


  ;; Display tags farther right
  (setq org-agenda-tags-column -102)

  ;;
  ;; Agenda sorting functions
  ;;
  (setq org-agenda-cmp-user-defined 'bh/agenda-sort)

   (defun org-agenda-current-subtree-or-region (only-todos)
     "Display an agenda view for the current subtree or region.
    With prefix, display only TODO-keyword items."
     (interactive "P")
     (let ((starting-point (point))
           header)
       (with-current-buffer (or (buffer-base-buffer (current-buffer))
                                (current-buffer))
         (if (use-region-p)
             (progn
               (setq header "Region")
               (put 'org-agenda-files 'org-restrict (list (buffer-file-name (current-buffer))))
               (setq org-agenda-restrict (current-buffer))
               (move-marker org-agenda-restrict-begin (region-beginning))
               (move-marker org-agenda-restrict-end
                            (save-excursion
                              ;; If point is at beginning of line, include
                              ;; heading on that line by moving forward 1.
                              (goto-char (1+ (region-end)))
                              (org-end-of-subtree))))
           ;; No region; restrict to subtree.
           (save-excursion
             (save-restriction
               ;; In case the command was called from an indirect buffer, set point
               ;; in the base buffer to the same position while setting restriction.
               (widen)
               (goto-char starting-point)
               (setq header "Subtree")
               (org-agenda-set-restriction-lock))))
         ;; NOTE: Unlike other agenda commands, binding `org-agenda-sorting-strategy'
         ;; around `org-search-view' seems to have no effect.
         (let ((org-agenda-sorting-strategy '(priority-down timestamp-up))
               (org-agenda-overriding-header header)))
           (org-agenda nil " "))
         (org-agenda-remove-restriction-lock t)
         (message nil)))

   (setq appt-display-diary nil)
   (appt-activate t)
   (setq appt-display-interval 5)
   (setq appt-message-warning-time 15)
   (setq appt-display-mode-line t)
   (display-time)
   (setq appt-display-format 'window)
   (setq appt-disp-window-function #'ora-appt-display)
   (run-at-time "1 hour" 3600 #'ora-org-agenda-to-appt)
   (remove-hook 'org-finalize-agenda-hook #'ora-org-agenda-to-appt)
   (add-hook 'org-finalize-agenda-hook #'ora-org-agenda-to-appt)

   (defun ora-appt-display (min-to-app new-time msg)
     "our little façade-function for ora-org-popup"
     (ora-org-popup (format "Appointment in %s minute(s)" min-to-app) msg
                    "~/Pictures/Icons/Gnome-appointment-soon.png") )

   (defun ora-org-agenda-to-appt ()
     "Erase all reminders and rebuild reminders for today from the agenda"
     (interactive)
     ;; (setq appt-time-msg-list nil)
     (org-agenda-to-appt))


   (defun ora-start-process (cmd)
     (start-process
      cmd nil shell-file-name
      shell-command-switch
      (format "nohup 1>/dev/null 2>/dev/null %s" cmd)))

   (defun ora-org-popup (title msg &optional icon sound)
     "Show a popup if we're on X, or echo it otherwise; TITLE is the title
              of the message, MSG is the context. Optionally, you can provide an ICON and
              a sound to be played"
     (interactive)
     (if (eq window-system 'x)
         (progn
           (notifications-notify
            :title title
            :body msg
            :app-icon icon
            :urgency 'low)
           (ora-start-process
            (concat "mplayer -really-quiet " sound " 2> /dev/null")))
       ;; text only version
       (message (concat title ": " msg))))

   (defun bh/org-agenda-to-appt ()
     (interactive)
     (setq appt-time-msg-list nil)
     (org-agenda-to-appt))

   ;; Rebuild the reminders everytime the agenda is displayed
   (add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

   ;; Activate appointments so we get notifications,
   ;; but only run this when emacs is idle for 15 seconds
   (run-with-idle-timer 15 nil (lambda () (appt-activate t)))

   ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
   (run-at-time "24:01" nil 'bh/org-agenda-to-appt)

  (setq org-agenda-directory "~/org/agenda/")
  (setq org-ref-directory "~/org/notes/")
  (setq org-meta-directory "~/org/meta/")

  (setq org-attach-directory "~/org/notes/data/")

  (setq org-file-inbox (concat org-agenda-directory      "inbox.org"))
  (setq org-file-journal (concat org-agenda-directory    "log.org"))
    (setq org-file-reference (concat org-ref-directory  "reference.org"))
  (setq org-file-calendar (concat org-agenda-directory   "calendar.org"))

  (setq org-archive-mark-done nil)

  (setq org-archive-location "~/org/agenda/log.org::datetree/* Completed Tasks")

    (setq org-capture-templates
          (quote (
          ("a" "Appointment" entry (file org-file-inbox) (file "~/.emacs.d/templates/event.tmplt")  :clock-in t :clock-resume t)
          ("t" "Task" entry (file org-file-inbox) (file "~/.emacs.d/templates/todo.tmplt") :clock-in t :clock-resume t)
          ("n" "Note" entry (file org-file-inbox) (file "~/.emacs.d/templates/note.tmplt") :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+datetree "~/org/agenda/log.org")  "** %^{Title}  :journal: \n\n %?" :clock-in t :clock-resume t)
          )))

     (defmacro bh/agenda-sort-test (fn a b)
        "Test for agenda sort"
        `(cond
                                              ; if both match leave them unsorted
          ((and (apply ,fn (list ,a))
                (apply ,fn (list ,b)))
           (setq result nil))
                                              ; if a matches put a first
          ((apply ,fn (list ,a))
           (setq result -1))
                                              ; otherwise if b matches put b first
          ((apply ,fn (list ,b))
           (setq result 1))
                                              ; if none match leave them unsorted
          (t nil)))

      (defmacro bh/agenda-sort-test-num (fn compfn a b)
        `(cond
          ((apply ,fn (list ,a))
           (setq num-a (string-to-number (match-string 1 ,a)))
           (if (apply ,fn (list ,b))
               (progn
                 (setq num-b (string-to-number (match-string 1 ,b)))
                 (setq result (if (apply ,compfn (list num-a num-b))
                                  -1
                                1)))
             (setq result -1)))
          ((apply ,fn (list ,b))
           (setq result 1))
          (t nil)))

      (defun bh/agenda-sort (a b)
        "Sorting strategy for agenda items.
      Late deadlines first, then scheduled, then non-late deadlines"
        (let (result num-a num-b)
          (cond
           ;; time specific items are already sorted first by org-agenda-sorting-strategy

           ;; non-deadline and non-scheduled items next
           ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

           ;; deadlines for today next
           ((bh/agenda-sort-test 'bh/is-due-deadline a b))

           ;; late deadlines next
           ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

           ;; scheduled items for today next
           ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

           ;; late scheduled items next
           ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

           ;; pending deadlines last
           ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

           ;; finally default to unsorted
           (t (setq result nil)))
          result))

      (defun bh/is-project-p ()
        "Any task with a todo keyword subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task has-subtask))))

      (defun bh/is-project-subtree-p ()
        "Any task with a todo keyword that is in a project subtree.
      Callers of this function already widen the buffer view."
        (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                    (point))))
          (save-excursion
            (bh/find-project-task)
            (if (equal (point) task)
                nil
              t))))

      (defun bh/is-task-p ()
        "Any task with a todo keyword and no subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task (not has-subtask)))))

      (defun bh/is-subproject-p ()
        "Any task which is a subtask of another project"
        (let ((is-subproject)
              (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
          (save-excursion
            (while (and (not is-subproject) (org-up-heading-safe))
              (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                (setq is-subproject t))))
          (and is-a-task is-subproject)))

      (defun bh/list-sublevels-for-projects-indented ()
        "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
        This is normally used by skipping functions where this variable is already local to the agenda."
        (if (marker-buffer org-agenda-restrict-begin)
            (setq org-tags-match-list-sublevels 'indented)
          (setq org-tags-match-list-sublevels nil))
        nil)

      (defun bh/list-sublevels-for-projects ()
        "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
        This is normally used by skipping functions where this variable is already local to the agenda."
        (if (marker-buffer org-agenda-restrict-begin)
            (setq org-tags-match-list-sublevels t)
          (setq org-tags-match-list-sublevels nil))
        nil)

      (defvar bh/hide-scheduled-and-waiting-next-tasks t)

      (defun bh/toggle-next-task-display ()
        (interactive)
        (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
        (when  (equal major-mode 'org-agenda-mode)
          (org-agenda-redo))
        (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

      (defun bh/skip-stuck-projects ()
        "Skip trees that are not stuck projects"
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (if (bh/is-project-p)
                (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                       (has-next ))
                  (save-excursion
                    (forward-line 1)
                    (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                      (unless (member "WAITING" (org-get-tags))
                        (setq has-next t))))
                  (if has-next
                      nil
                    next-headline)) ; a stuck project, has subtasks but no next task
              nil))))

      (defun bh/skip-non-stuck-projects ()
        "Skip trees that are not stuck projects"
        ;; (bh/list-sublevels-for-projects-indented)
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (if (bh/is-project-p)
                (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                       (has-next ))
                  (save-excursion
                    (forward-line 1)
                    (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                      (unless (member "WAITING" (org-get-tags))
                        (setq has-next t))))
                  (if has-next
                      next-headline
                    nil)) ; a stuck project, has subtasks but no next task
              next-headline))))

      (defun bh/skip-non-projects ()
        "Skip trees that are not projects"
        ;; (bh/list-sublevels-for-projects-indented)
        (if (save-excursion (bh/skip-non-stuck-projects))
            (save-restriction
              (widen)
              (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                (cond
                 ((bh/is-project-p)
                  nil)
                 ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
                  nil)
                 (t
                  subtree-end))))
          (save-excursion (org-end-of-subtree t))))

      (defun bh/skip-non-tasks ()
        "Show non-project tasks.
      Skip project and sub-project tasks, habits, and project related tasks."
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (cond
             ((bh/is-task-p)
              nil)
             (t
              next-headline)))))

      (defun bh/skip-project-trees-and-habits ()
        "Skip trees that are projects"
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((bh/is-project-p)
              subtree-end)
             ((org-is-habit-p)
              subtree-end)
             (t
              nil)))))

      (defun bh/skip-projects-and-habits-and-single-tasks ()
        "Skip trees that are projects, tasks that are habits, single non-project tasks"
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (cond
             ((org-is-habit-p)
              next-headline)
             ((and bh/hide-scheduled-and-waiting-next-tasks
                   (member "WAITING" (org-get-tags)))
              next-headline)
             ((bh/is-project-p)
              next-headline)
             ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
              next-headline)
             (t
              nil)))))

      (defun bh/skip-project-tasks-maybe ()
        "Show tasks related to the current restriction.
      When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
      When not restricted, skip project and sub-project tasks, habits, and project related tasks."
        (save-restriction
          (widen)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (next-headline (save-excursion (or (outline-next-heading) (point-max))))
                 (limit-to-project (marker-buffer org-agenda-restrict-begin)))
            (cond
             ((bh/is-project-p)
              next-headline)
             ((org-is-habit-p)
              subtree-end)
             ((and (not limit-to-project)
                   (bh/is-project-subtree-p))
              subtree-end)
             ((and limit-to-project
                   (bh/is-project-subtree-p)
                   (member (org-get-todo-state) (list "NEXT")))
              subtree-end)
             (t
              nil)))))

      (defun bh/skip-project-tasks ()
        "Show non-project tasks.
      Skip project and sub-project tasks, habits, and project related tasks."
        (save-restriction
          (widen)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((bh/is-project-p)
              subtree-end)
             ((org-is-habit-p)
              subtree-end)
             ((bh/is-project-subtree-p)
              subtree-end)
             (t
              nil)))))

      (defun bh/skip-non-project-tasks ()
        "Show project tasks.
      Skip project and sub-project tasks, habits, and loose non-project tasks."
        (save-restriction
          (widen)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (cond
             ((bh/is-project-p)
              next-headline)
             ((org-is-habit-p)
              subtree-end)
             ((and (bh/is-project-subtree-p)
                   (member (org-get-todo-state) (list "NEXT")))
              subtree-end)
             ((not (bh/is-project-subtree-p))
              subtree-end)
             (t
              nil)))))

      (defun bh/skip-projects-and-habits ()
        "Skip trees that are projects and tasks that are habits"
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((bh/is-project-p)
              subtree-end)
             ((org-is-habit-p)
              subtree-end)
             (t
              nil)))))

      (defun bh/skip-non-subprojects ()
        "Skip trees that are not projects"
        (let ((next-headline (save-excursion (outline-next-heading))))
          (if (bh/is-subproject-p)
              nil
            next-headline)))

      ;; Show 20 minute clocking gaps. Hit "v c" in the agenda view
      (setq org-agenda-clock-consistency-checks
            '(:max-duration "4:00"
                            :min-duration 0
                            :max-gap 30
                            :gap-ok-around ("4:00" "11:00" "19:00" "20:00" "21:00")))

      (defun bh/widen ()
        (interactive)
        (if (equal major-mode 'org-agenda-mode)
            (progn
              (org-agenda-remove-restriction-lock)
              (when org-agenda-sticky
                (org-agenda-redo)))
          (widen)))

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
                'append)

      (defun bh/skip-non-archivable-tasks ()
        "Skip trees that are not available for archiving."
        (save-restriction
          (widen)
          ;; Consider only tasks with done todo headings as archivable candidates
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
                (subtree-end (save-excursion (org-end-of-subtree t))))
            (if (member (org-get-todo-state) org-todo-keywords-1)
                (if (member (org-get-todo-state) org-done-keywords)
                    (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                           (a-month-ago (* 60 60 24 (+ daynr 1)))
                           (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                           (this-month (format-time-string "%Y-%m-" (current-time)))
                           (subtree-is-current (save-excursion
                                                 (forward-line 1)
                                                 (and (< (point) subtree-end)
                                                      (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                      (if subtree-is-current
                          subtree-end ; Has a date in this month or last month, skip it
                        nil))  ; available to archive
                  (or subtree-end (point-max)))
              next-headline))))

      (defun bh/is-not-scheduled-or-deadline (date-str)
        (and (not (bh/is-deadline date-str))
             (not (bh/is-scheduled date-str))))

      (defun bh/is-due-deadline (date-str)
        (string-match "Deadline:" date-str))

      (defun bh/is-late-deadline (date-str)
        (string-match "\\([0-9]*\\) d\. ago:" date-str))

      (defun bh/is-pending-deadline (date-str)
        (string-match "In \\([^-]*\\)d\.:" date-str))

      (defun bh/is-deadline (date-str)
        (or (bh/is-due-deadline date-str)
            (bh/is-late-deadline date-str)
            (bh/is-pending-deadline date-str)))

      (defun bh/is-scheduled (date-str)
        (or (bh/is-scheduled-today date-str)
            (bh/is-scheduled-late date-str)))

      (defun bh/is-scheduled-today (date-str)
        (string-match "Scheduled:" date-str))

      (defun bh/is-scheduled-late (date-str)
        (string-match "Sched\.\\(.*\\)x:" date-str))

      (defun bh/find-project-task ()
        "Move point to the parent (project) task if any"
        (save-restriction
          (widen)
          (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
            (while (org-up-heading-safe)
              (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                (setq parent-task (point))))
            (goto-char parent-task)
            parent-task)))

      (defun bh/verify-refile-target ()
        "Exclude todo keywords with a done state from refile targets"
        (not (member (nth 2 (org-heading-components)) org-done-keywords)))

      (defun bh/clock-in-to-next (kw)
        "Switch a task from TODO to NEXT when clocking in.
                         Skips capture tasks, projects, and subprojects.
                         Switch projects and subprojects from NEXT back to TODO"
        (when (not (and (boundp 'org-capture-mode) org-capture-mode))
          (cond
           ((and (member (org-get-todo-state) (list "TODO"))
                 (bh/is-task-p))
            "NEXT")
           ((and (member (org-get-todo-state) (list "NEXT"))
                 (bh/is-project-p))
            "TODO"))))

)
