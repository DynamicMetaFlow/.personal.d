(use-package exwm
;  :if (string= (getenv "EXWM_ENABLE") "t")
  :config
  (require 'exwm)
  (require 'exwm-randr)
                                        ;      (require 'exwm-systemtray)

  (setq display-time-mode t)
  (setq display-battery-mode nil)

  (setq fringe-mode '(7 . 1))

  (setq use-dialog-box nil)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)

  (setq exwm-workspace-number 8)

  (defun exwm-workspace-number-to-string (number)
    (number-to-string (1- number)))

  (setq exwm-workspace-index-map #'exwm-workspace-number-to-string)

  (dotimes (i 8)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create (1- ,i)))))

  (ido-mode 0)

  (setq display-time-default-load-average nil)

  (setq window-divider-default-bottom-width 2
        window-divider-default-right-width 2)

  (setq exwm-manage-configurations '((t char-mode t)))

  (defvar exwm-toggle-workspace 0
    "Previously selected workspace. Used with `exwm-jump-to-last-exwm'.")

  (defun exwm-jump-to-last-exwm ()
    (interactive)
    (exwm-workspace-switch exwm-toggle-workspace))

  (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
    (setq exwm-toggle-workspace exwm-workspace-current-index))

  (defun my/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))

  (add-hook 'exwm-update-title-hook 'my/exwm-rename-buffer-to-title)

  (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

  (defun my-exwm-workspace-display-current ()
    "Display current workspace index."
    (interactive)
    (let ((message-log-max))
      (message (elt exwm-workspace--switch-history
                    exwm-workspace-current-index))))

  (defvar exwm-workspace-switch-wrap t
    "Whether `exwm-workspace-next' and `exwm-workspace-prev' should wrap.")

  (defun exwm-workspace-next ()
    "Switch to next exwm-workspaceective (to the right)."
    (interactive)
    (let* ((only-workspace? (equal exwm-workspace-number 1))
           (overflow? (= exwm-workspace-current-index
                         (1- exwm-workspace-number))))
      (cond
       (only-workspace? nil)
       (overflow?
        (when exwm-workspace-switch-wrap
          (exwm-workspace-switch 0)))
       (t (exwm-workspace-switch  (1+ exwm-workspace-current-index))))))

  (defun gpastel-exwm-counsel-yank-pop ()
    "Same as `counsel-yank-pop' and paste into exwm buffer."
    (interactive)
    (let ((inhibit-read-only t)
          ;; Make sure we send selected yank-pop candidate to
          ;; clipboard:
          (yank-pop-change-selection t))
      (call-interactively #'counsel-yank-pop))
    (when (derived-mode-p 'exwm-mode)
      ;; https://github.com/ch11ng/exwm/issues/413#issuecomment-386858496
      (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
      ;; (exwm-input--fake-key ?\C-v)
      ))

  (exwm-input-set-key (kbd "M-y")
                      #'gpastel-exwm-counsel-yank-pop)


  (defun exwm-counsel-yank-pop ()
    "Same as `counsel-yank-pop' and paste into exwm buffer."
    (interactive)
    (let ((inhibit-read-only t)
          ;; Make sure we send selected yank-pop candidate to
          ;; clipboard:
          (yank-pop-change-selection t))
      (call-interactively #'counsel-yank-pop))
    (when (derived-mode-p 'exwm-mode)
      (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
      (exwm-input--fake-key ?\C-v)))


  (defun exwm-workspace-prev ()
    "Switch to next exwm-workspaceective (to the right)."
    (interactive)
    (let* ((only-workspace? (equal exwm-workspace-number 1))
           (overflow? (= exwm-workspace-current-index 0)))
      (cond
       (only-workspace? nil)
       (overflow?
        (when exwm-workspace-switch-wrap
          (exwm-workspace-switch (1- exwm-workspace-number))))
       (t (exwm-workspace-switch  (1- exwm-workspace-current-index))))))

  (defun mf/get-auto-xrandr-line (display)
    "Returns the parameter for xrandr to automatically configure the display."
    (concat "--output " display " --auto"))

  (defun mf/generate-xrandr-params ()
    "Generate the parameters for xrandr."
    (let ((xrandrfile "~/.xrandrrc"))
      (if (not (file-exists-p xrandrfile))
          (apply 'concat (mapcar 'mf/get-auto-xrandr-line (mf/get-monitor-list)))
        (with-temp-puffer
         (insert-file-contents xrandrfile)
         (buffer-to-string)))))

  (defun app/configure-displays ()
    "Configure the attached displays"
    (interactive)
    (let ((xrandr-params (mf/generate-xrandr-params)))
      (start-process-shell-command "xrandr" nil (concat "xrandr " xrandr-params))))


  ;; Quick swtiching between workspaces
  (defvar exwm-toggle-workspace 0
    "Previously selected workspace. Used with `exwm-jump-to-last-exwm'.")
  (defun exwm-jump-to-last-exwm ()
    (interactive)
    (exwm-workspace-switch exwm-toggle-workspace))

  (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
    (setq exwm-toggle-workspace exwm-workspace-current-index))

  (defun mf/make-workspace-list (monitors workspaces)
    "Return a list for exwm to use for workspace assignment."
    (let ((result '()))
      (dotimes (count (* (length monitors) workspaces) result)
        (setq result (append result
                             (list (+ count 1)
                                   (nth (if (evenp count) count (- count 1))
                                        monitors)))))))
  (defun mf/get-monitor-list ()
    "Get a list of connected displays."
    (let ((xrandr (split-string (shell-command-to-string "xrandr") "\n" ))
          (result '()))
      (dotimes (counter (length xrandr) result)
        (let ((monitor (mf/monitor-from-line (nth counter xrandr))))
          (when monitor
            (setq result (append result (list monitor))))))))

  (setq exwm-workspaces-per-monitor 4)

  (defun mf/monitor-from-line (line)
    "Look at a line and return the monitor if it is a matching xrandr line."
    (save-match-data
      (and (string-match " connected" line)
           (string-match "^[a-zA-Z]+-[0-9]+" line)
           (match-string 0 line ))))


  (defun my-exwm-workspaces ()
    "Calculates the number of workspaes and assigns them to monitors."
    (interactive)

    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist
          (mf/make-workspace-list (mf/get-monitor-list) exwm-workspaces-per-monitor))
    (add-hook 'exwm-randr-screen-change-hook 'app/configure-displays)


    (dotimes (value (* exwm-workspaces-per-monitor (length (mf/get-monitor-list))))
      (let ((i (+ 1 value)))
        (exwm-input-set-key
         (kbd (format "s-%d" i))
         `(lambda () (interactive)
            (exwm-workspace-switch-create ,i)))))
    (exwm-randr-enable))


  (defun my-exwm-define-key-chords ()
    "Define local key chords for Exwm buffer."
    (map-keymap
     (lambda (event-type key-chord-map)
       (when (eq event-type 'key-chord)
         (map-keymap
          (lambda (key _)
            (define-key exwm-mode-map (string key)
              (lambda ()
                (interactive)
                (exwm-input--fake-key key))))
          key-chord-map)))
     (current-global-map)))

  (defun my/trim-non-chrome ()
    (delete-if-not (apply-partially 'string-match "- Google Chrome$")
                   ido-temp-list))

  (add-hook 'exwm-manage-finish-hook
            (defun my/exwm-manage-hook ()
              (when (string-match "Google-chrome" exwm-class-name)
                                        ;                (exwm-workspace-move-window 0)
                                        ;                (exwm-layout-hide-mode-line)
                (setq ido-make-buffer-list-hook 'my/trim-non-chrome))))

  (add-hook 'exwm-update-title-hook
            (defun my/exwm-title-hook ()
              (when (string-match "Google-chrome" exwm-class-name)
                (exwm-workspace-rename-buffer exwm-title))))

  (setq browse-url-chrome-arguments '("--new-window"))

  (add-hook 'exwm-workspace-switch-hook #'my-exwm-workspace-display-current)
  (advice-add 'exwm-workspace-add :after #'my-exwm-workspace-display-current)
  (advice-add 'exwm-workspace-delete :after #'my-exwm-workspace-display-current)


  (window-divider-mode)

  (defun ivy-ignore-exwm-buffers (str)
    (let ((buf (get-buffer str)))
      (when buf
        (with-current-buffer buf
          (or
           (file-remote-p (or (buffer-file-name) default-directory))
           (eq major-mode 'exwm-mode))))))

  (defun ivy-ignore-non-exwm-buffers (str)
    (let ((buf (get-buffer str)))
      (if buf
          (with-current-buffer buf
            (or
             (file-remote-p (or (buffer-file-name) default-directory))
             (not (eq major-mode 'exwm-mode))))
        t)))

  (defun ivy-switch-buffer-exwm ()
    "Like ivy-switch-buffer but only shows EXWM buffers."
    (interactive)
    (let ((ivy-ignore-buffers (append ivy-ignore-buffers '(ivy-ignore-non-exwm-buffers))))
      (ivy-switch-buffer)))

  (defun ivy-switch-buffer-non-exwm ()
    "Like ivy-switch-buffer but hides all EXWM buffers."
    (interactive)
    (let ((ivy-ignore-buffers (append ivy-ignore-buffers '(ivy-ignore-exwm-buffers))))
      (ivy-switch-buffer)))

  (setq exwm-input-line-mode-passthrough t)

  (defun exwm-input-line-mode ()
    "Set exwm window to line-mode and show mode line"
    (call-interactively #'exwm-input-grab-keyboard)
    (exwm-layout-show-mode-line))

  (defun exwm-input-char-mode ()
    "Set exwm window to char-mode and hide mode line"
    (call-interactively #'exwm-input-release-keyboard)
    (exwm-layout-hide-mode-line))

  (defun exwm-input-toggle-mode ()
    "Toggle between line- and char-mode"
    (with-current-buffer (window-buffer)
      (when (eq major-mode 'exwm-mode)
        (if (equal (second (second mode-line-process)) "line")
            (exwm-input-char-mode)
          (exwm-input-line-mode)))))

  (defun exwm-input-set-global-key (key function)
    "Add KEY to `exwm-input-prefix-keys' and bind FUNCTION to KEY
          in exwm keymap"
    (cl-pushnew (elt key 0) exwm-input-prefix-keys)
    (exwm-input-set-key key function))

  (defun my/switch-to-last-buffer ()
    "Switch to last open buffer in current window."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

  (define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

  (push ?\s-  exwm-input-prefix-keys)

  (defvar exwm-input-prefix-keys-extra nil)


  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])

          ;; cut/paste
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))

  (defun my-exwm-keybindings ()
    "Add the key bindings for exwm."
    (exwm-input-set-key (kbd "<print>") #'desktop-environment-screenshot)

    ;; (exwm-input-set-key (kbd "C-SPC")
    ;;                     (lambda ()
    ;;                       (interactive)
    ;;                       (exwm-input-line-mode)
    ;;                       (hydra-master/body)))

    (exwm-input-set-key (kbd "C-SPC")
                        (lambda ()
                          (interactive)
                          (exwm-input-line-mode)
                          (modalka-mode-hydra)))

  (exwm-input-set-key (kbd "C-4")
                        (lambda ()
                          (interactive)
                          (exwm-input-line-mode)
                          (my/org-capture-appt)))
  (exwm-input-set-key (kbd "C-1")
                        (lambda ()
                          (interactive)
                          (exwm-input-line-mode)
                          (my/org-capture-task)))

  (exwm-input-set-key (kbd "C-2")
                        (lambda ()
                          (interactive)
                          (exwm-input-line-mode)
                          (my/org-capture-journal)))


  (exwm-input-set-key (kbd "C-3")
                        (lambda ()
                          (interactive)
                          (exwm-input-line-mode)
                          (my/org-capture-note)))


  (exwm-input-set-key (kbd "s-p")   'my/switch-to-last-buffer)
  (exwm-input-set-key (kbd "s-SPC") 'exwm-jump-to-last-exwm)
  (exwm-input-set-key (kbd "s-<tab>") 'ivy-switch-buffer-exwm)


    (exwm-input-set-key (kbd "s-f")   'toggle-single-window)

    (exwm-input-set-key (kbd "s-,")   'winner-undo)
    (exwm-input-set-key (kbd "s-.")   'winner-redo)

    (exwm-input-set-key (kbd "s-r") 'exwm-reset)
    (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)

    (exwm-input-set-key (kbd "s-l") 'windmove-right)
    (exwm-input-set-key (kbd "s-k") 'windmove-left)
    (exwm-input-set-key (kbd "s-i") 'windmove-up)
    (exwm-input-set-key (kbd "s-o") 'windmove-down)


    (exwm-input-set-key (kbd "s-L") 'buf-move-right)
    (exwm-input-set-key (kbd "s-K") 'buf-move-left)
    (exwm-input-set-key (kbd "s-I") 'buf-move-up)
    (exwm-input-set-key (kbd "s-O") 'buf-move-down)

    (exwm-input-set-key (kbd "s-x") 'exwm-input-toggle-keyboard))

  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("EXWM" (mode . exwm-mode))))))
                                          ;       ("org-mode" (mode . org-mode))
                                          ;       ("git" (mode . magit-status-mode))
                                          ;       ("dired" (mode . dired-mode))
                                          ;       ("emacs" (or
                                          ;                (name . "^\\*scratch\\*$")
                                          ;                (name . "^\\*Messages\\*$")
                                          ;                (name . "^\\*Bookmark List\\*$")
                                          ;                (name . "^\\*GNU Emacs\\*$")))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))


  (define-ibuffer-column size-h
    (:name "Size")
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (define-ibuffer-column exwm-class
    (:name "Class")
    (cond
     (exwm-class-name (format "%s" exwm-class-name))
     (t (format "%s" ""))))

  ;; Needs work to look good, major-mode is not equal to ibuffer-formats mode
  (define-ibuffer-column exwm-mode
    (:name "EXWM-Mode")
    (cond
     ((string-equal major-mode "exwm-mode") (format "%s" exwm-class-name))
     (t (format "%s" mode-name))))

  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 50 50 :left :elide)
                " "
                (size-h 16 16 :right)
                " "
                (exwm-mode 18 18 :left :elide)
                " " filename-and-process)
          (mark modified read-only locked " "
                (name 50 50 :left :elide)
                " "
                (size-h 16 16 :right)
                " "
                (mode 18 18 :left :elide)
                (exwm-class 18 18 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))

  (defun my-counsel-ibuffer-by-exwm-class-name ()
    "`counsel-ibuffer' limited to Exwm buffers of same X class."
    (interactive)
    (require 'ibuffer)
    (cl-letf*
        ((class-name exwm-class-name)
         (get-buffers-function
          (symbol-function 'counsel-ibuffer--get-buffers))
         ((symbol-function 'counsel-ibuffer--get-buffers)
          (lambda ()
            (--filter (with-current-buffer (cdr it)
                        (and (eq major-mode 'exwm-mode)
                             (string-equal exwm-class-name class-name)))
                      (funcall get-buffers-function)))))
      (counsel-ibuffer)))

  (defvar exwm-connected-displays 3
    "Number of connected displays.")

  ;; Update exwm-randr-workspace-output-plist with 2 or 3 outputs named
  ;; 'primary' and 'other-1'/'other-2'.
  ;; With 3 outputs connected the first workspace will be primary,
  ;; second workspace goes to 'other-2' and all others to 'other-1'.
  ;; With 2 outputs, first workspace is 'primary' display and rest 'other-1'.
  ;; And with only one connected output, primary has all workspaces.
  (defun my/exwm-randr-screen-change ()
    (let* ((connected-cmd "xrandr -q|awk '/ connected/ {print $1}'")
           (connected (process-lines "bash" "-lc" connected-cmd))
           (primary (car connected))  ; Primary display is always first in list
           (other-1 (cadr connected))
           (other-2 (caddr connected)))
      (setq exwm-connected-displays (length connected))
      (setq exwm-randr-workspace-monitor-plist
            (append (list 0 primary)
                    (list 1 (or other-2 other-1 primary))
                    (mapcan (lambda (i) (list i (or other-1 other-2 primary)))
                            (number-sequence 2 exwm-workspace-number))))
      (exwm-randr-refresh)
      (message "Randr: %s monitors refreshed." (string-join connected ", "))))

  (add-hook 'exwm-randr-screen-change-hook #'my/exwm-randr-screen-change)

  (defun app/autostart (application)
    "Add an application to autostart."
    (add-hook 'exwm-init-hook
              `(lambda ()
                 (start-process-shell-command "autostart-process" nil ,application))))

  (defun my-exwm-autostart ()
    "Add applications that will be loaded after exwm init is done."
    (mapcar (lambda (program) (app/autostart program)) exwm-autostart))


  (setq exwm-autostart
        (list
;         "thinkpad-dock off"
;         "compton -b"
;         "thinkpad-touchpad off"
;         "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
;         "/usr/lib/gpaste/gpaste-daemon"
;         "pamac-tray"
;         "redshift-gtk"
;         "kdeconnect-indicator"
;         "autorandr --change"
;         "thinkpad-dock on"
         ))


  (add-hook 'after-init-hook
            (lambda ()
              (exwm-randr-enable)
;              (exwm-systemtray-enable)
              (exwm-input--update-global-prefix-keys)
              (my-exwm-keybindings)
;             (my-exwm-autostart)
;              (my-exwm-workspaces)
              (exwm-enable)
              t)))
