(defhydra hydra-common  (:color blue
                                :hint nil)
  ("RET"   counsel-linux-app nil)
  ("SPC"   counsel-M-x nil)

  ("Q" kill-this-buffer nil)

  ("<tab>" ivy-switch-buffer-non-exwm)
  ("s-<tab>"  ivy-switch-buffer-exwm)

  ("@"   org-capture nil)
  ("W"   hydra-window/body nil)

  ("0" delete-window-balance)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)

  (";" ace-swap-window)
  ("#" hycontrol-windows-grid))

(defhydra hydra-master (
                        :color blue
                        :column 1
                        :inherit (hydra-common/heads))
  "Master"
  ("a" hydra-applications/body "apps")
  ("b" hydra-bookmarks/body "bookmarks")
  ("c" quick-calc "calc")
  ("e" hydra-emacs/body "emacs")
  ("f" hydra-file/body "file")
  ("l" hydra-spell/body "spell")
  ("m" boon-mode-hydra "major")
  ("M" hydra-minor/body nil)
  ("p" hydra-projects/body "projects")
  ("s" hydra-search/body "search")
  ("t" hydra-terminal/body "term")
  ("x" hydra-text/body "text"))

(defhydra hydra-emacs (:color blue :hint nil :inherit (hydra-common/heads))

  "
                                                                         ╭───────┐
     Execute       Packages         Help                     Misc        │ Emacs │
  ╭──────────────────────────────────────────────────────────────────────┴───────╯
    [_x_] counsel M-x [_p_] list      [_f_] describe function [_t_] change theme
    [_e_] exit        [_i_] install   [_v_] describe variable [_l_] list emacs process
    [_s_] system      [_u_] upgrade   [_m_] info manual       [_c_] init time
     ^ ^               ^ ^            [_k_] bindings          [_o_] unbound commands
     ^ ^               ^ ^            [_b_] personal bindings [_y_] emacs colors
     ^ ^               ^ ^             ^ ^                    [_z_] list faces
     ^ ^               ^ ^             ^ ^
  --------------------------------------------------------------------------------
        "
  ("C-h b" counsel-descbinds "bindings")
  ("f" counsel-describe-function)
  ("v" counsel-describe-variable)
  ("b" describe-personal-keybindings)
  ("c" emacs-init-time)
  ("i" package-install)
  ("k" counsel-descbinds)
  ("l" list-processes)
  ("m" info-display-manual)
  ("p" paradox-list-packages)
  ("t" counsel-load-theme)
  ("u" paradox-upgrade-packages)
  ("o" smex-show-unbound-commands)
  ("y" counsel-colors-emacs)
  ("z" counsel-faces)
  ("x" counsel-M-x)
  ("e" save-buffers-kill-emacs)
  ("s" hydra-system/body nil)
  )

(defhydra hydra-terminal (:color blue :hint nil :inherit (hydra-common/heads))

  "
                                                                        ╭──────────┐
     Terminals                     System                               │ Terminal │
  ╭─────────────────────────────────────────────────────────────────────┴──────────╯
    [_s_] new multi-term           [_c_] shell command
    [_n_] next multi-term          [_a_] aync shell command
    [_p_] previous multi-term      [_m_] man page
    [_d_] dedicated multi-term     [_l_] list system process
    [_e_] eshell
  --------------------------------------------------------------------------------
        "
  ("a" async-shell-command)
  ("c" shell-command)
  ("e" eshell)
  ("m" man)
  ("l" proced)
  ("s" multi-term)
  ("n" multi-term-next)
  ("p" multi-term-previous)
  ("d" multi-term-dedicated-toggle))

(defhydra hydra-file (:color blue :hint nil :inherit (hydra-common/heads))

  "
                                                                          ╭──────┐
       Ivy                    Dired            Hydras                     │ File │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
    [_f_] open file            [_d_] dired     [_ht_] text
    [_e_] open file extern                   [_hs_] spell
    [_r_] open recentf

  --------------------------------------------------------------------------------
        "
  ("f" counsel-find-file)
  ("e" counsel-find-file-extern)
  ("r" counsel-recentf "recent")
  ("d" dired-jump)
  ("hs" hydra-spell/body nil  :color blue)
  ("ht" hydra-text/body nil   :color blue)
  )

(defhydra hydra-narrow (:color blue :hint nil :inherit (hydra-common/heads))

  "
                                                                        ╭────────┐
      Narrow                                                            │ Narrow │
  ╭─────────────────────────────────────────────────────────────────────┴────────╯
    [_f_] narrow to defun
    [_p_] narrow to page
    [_s_] narrow to subtree
    [_r_] narrow to region

    [_w_] widen
  --------------------------------------------------------------------------------
        "
  ("f" narrow-to-defun)
  ("p" narrow-to-page)
  ("s" org-narrow-to-subtree)
  ("c" org-narrow-to-block)
  ("r" narrow-to-region)
  ("w" widen))

(defhydra hydra-spell (:color blue :hint nil :inherit (hydra-common/heads))

  "
                                                                         ╭───────┐
      Flyspell               Ispell                      Gtranslate      │ Spell │
  ╭──────────────────────────────────────────────────────────────────────┴───────╯
    [_k_] correct word       [_w_] check word            [_g_] en ⇆ es
    [_n_] next error                                   [_G_] any lang
    [_f_] toggle flyspell
    [_p_] toggle prog mode
  --------------------------------------------------------------------------------
        "
  ("w" ispell-word)
  ("d" ispell-change-dictionary)
  ("g" google-translate-smooth-translate)
  ("G" google-translate-query-translate)
  ("f" flyspell-mode)
  ("p" flyspell-prog-mode)
  ("k" flyspell-correct-word-generic)
  ("n" flyspell-goto-next-error))

(defhydra hydra-text (:color blue :hint nil :inherit (hydra-common/heads))


  "
                                                                               ╭──────┐
        Size  Toggle              Unicode                        Do            │ Text │
       ╭───────────────────────────────────────────────────────────────────────┴──────╯
         _k_  [_f_] fill column     [_d_] unicode character           [_a_] align with regex
         ^↑^  [_h_] hidden chars    [_e_] evil digraphs table         [_w_] remove trailing ' '
         ^ ^  [_l_] line numbers    [_s_] specific code block         [_n_] count words
         ^↓^  [_t_] trailing ' '    [_u_] unicode character           [_i_] lorem ipsum
         _j_  [_v_] font space      [_p_] character code              [_x_] comment box
         ^ ^  [_c_] comment          ^ ^                              [_q_] boxquote
         ^ ^  [_b_] multibyte chars  ^ ^                              [_m_] iedit (multiple)
         ^ ^   ^ ^                   ^ ^                              [_r_] expand region
         ^ ^   ^ ^                   ^ ^                              [_U_] tabs to spaces
       --------------------------------------------------------------------------------
             "
  ("a" align-regexp)
  ("b" toggle-enable-multibyte-characters)
  ("c" comment-line)
  ("d" insert-char)
  ("e" evil-ex-show-digraphs)
  ("f" fci-mode)
  ("h" whitespace-mode)
  ("i" lorem-ipsum-insert-paragraphs)
  ("k" text-scale-increase :color red)
  ("j" text-scale-decrease :color red)
  ("l" linum-mode)
  ("n" count-words)
  ("m" iedit)
  ("p" describe-char)
  ("r" er/expand-region)
  ("s" charmap)
  ("t" joe-toggle-show-trailing-whitespace)
  ("u" counsel-unicode-char)
  ("v" variable-pitch-mode)
  ("w" whitespace-cleanup)
  ("U" untabify)
  ("q" hydra-boxquote/body)
  ("x" comment-box))

(defhydra hydra-git (:color blue :hint nil :inherit (hydra-common/heads))

  "
                                                                           ╭─────┐
     Magit                          VC                    Timemachine      │ Git │
  ╭────────────────────────────────────────────────────────────────────────┴─────╯
    [_s_] status              [_d_] diffs between revisions  [_t_] timemachine
    [_B_] blame mode          [_b_] edition history
    [_l_] file log
  --------------------------------------------------------------------------------
        "
  ("B" magit-blame)
  ("b" vc-annotate)
  ("d" vc-diff)
  ("l" magit-log-buffer-file)
  ("s" magit-status)
  ("t" git-timemachine))

(defhydra hydra-window (
                        :color red
                        :hint  nil
                        :inherit (hydra-common/heads))
  "
                                                                       ╭──────────┐
    Window         Switch           View                               │  Window  │
  ╭────────────────────────────────────────────────────────────────────┴──────────╯
       ↑          [_b_] ibuffer     [_s_] save
       _i_          [_p_] projectile  [_d_] delete
   ← _k_   _l_ →      [_e_] exwm        [_v_] view
       _o_
       ↓

     ^ ^
  --------------------------------------------------------------------------------
      "
  ("i" windmove-up)
  ("o" windmove-down)
  ("k" windmove-left)
  ("l" windmove-right)

  ("I" buf-move-up)
  ("O" buf-move-down)
  ("K" buf-move-left)
  ("L" buf-move-right)

  ("b" ibuffer)
  ("p" counsel-projectile-switch-to-buffer)
  ("e" ivy-switch-buffer-exwm)

  ("s"  ivy-push-view)
  ("d"  ivy-pop-view)
  ("v"  ivy-switch-view)

  ("0" delete-window-balance)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)

  (";" ace-swap-window "swap")
  ("@" hycontrol-windows-grid)
  )

(defhydra hydra-jump (:color blue :hint nil :inherit (hydra-common/heads))

  "
                                                                          ╭──────┐
    Window          WordChar        Line         iSearch                  │ Jump │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
    [_w_] jump        [_j_] word         [_l_] jump     [_i_] jump
    [_d_] close       [_p_] all words    [_y_] copy
    [_z_] maximize    [_b_] subword      [_m_] move
    [_s_] swap        [_c_] char         [_v_] copy region
     ^ ^              [_a_] two chars
  --------------------------------------------------------------------------------
        "
  ("w" ace-window)
  ("d" ace-delete-window)
  ("z" ace-maximize-window)
  ("s" ace-swap-window)
  ("j" avy-goto-word-1)
  ("p" avy-goto-word-0)
  ("b" avy-goto-subword-0)
  ("c" avy-goto-char)
  ("a" avy-goto-char-2)
  ("l" avy-goto-line)
  ("y" avy-copy-line)
  ("m" avy-move-line)
  ("v" avy-copy-region)
  ("i" avy-isearch)
  )

(defhydra hydra-register (:color blue :hint nil :inherit (hydra-common/heads))


  "
                                                                          ╭──────────┐
         Logs                        Registers                Undo        │ Register │
      ╭───────────────────────────────────────────────────────────────────┴──────────╯
        [_c_] commands history       [^e^] registers        [_u_] undo tree
        [_o_] messages
        [_l_] lossage (keystrokes)
        [_d_] diff buffer with file
      --------------------------------------------------------------------------------
            "
  ("d" joe-diff-buffer-with-file)
  ("k" counsel-yank-pop)
  ("l" view-lossage)
  ("c" counsel-command-history)
  ("m" evil-show-marks)
  ("o" view-echo-area-messages)
  ("r" evil-show-registers)
  ("u" undo-tree-visualize))

(defhydra hydra-search (:color blue :hint nil :inherit (hydra-common/heads))

  "
                                                                            ╭────────┐
         Files                             Buffer                           │ Search │
      ╭─────────────────────────────────────────────────────────────────────┴────────╯
        [_a_] regex search (Ag)           [_b_] by word
        [_r_] regex search (rg)           [_o_] by word (opened buffers)
        [_p_] regex search (pt)           [_w_] by word (multi)
        [_g_] regex search (grep)         [_h_] by word (grep or swiper)
        [^f^] find                        [_t_] tags & titles
        [_l_] locate                      [_s_] semantic
      --------------------------------------------------------------------------------
            "
  ("a" (let ((current-prefix-arg "-."))
         (call-interactively 'counsel-ag)))
  ("r" (let ((current-prefix-arg "-."))
         (call-interactively 'counsel-rg)))
  ("p" (let ((current-prefix-arg "-."))
         (call-interactively 'counsel-pt)))
  ("g" rgrep)
  ("l" counsel-locate)
  ("b" swiper)
  ("o" swiper-all)
  ("h" counsel-grep-or-swiper)
  ("t" counsel-imenu)
  ("s" counsel-semantic)
  ("w" swiper-multi))

(defhydra hydra-bookmarks (
                           :color red
                           :hint nil
                           :inherit (hydra-common/heads))

  "
                                                                     ╭───────────┐
         List                          Do                            │ Bookmarks │
  ╭──────────────────────────────────────────────────────────────────┴───────────╯
    [_l_] list bookmarks            [_j_] jump to a bookmark
     ^ ^                            [_m_] set bookmark at point
     ^ ^                            [_s_] save bookmarks
  --------------------------------------------------------------------------------
      "
  ("l" counsel-bookmark)
  ("j" bookmark-jump)
  ("m" bookmark-set)
  ("s" bookmark-save))

(defhydra hydra-help (:exit t :columns 4)
  "Help"
  ("f" counsel-apropos "Function search")
  ("k" view-lossage "View Keystrokes")
  ("c" find-function "Function code")
  ("P" esup "Profile")
  ("h" helpful-at-point "Help at point")
  ("p" find-function-at-point "Function at Point")
  ("l" find-library "Library source"))

(defhydra hydra-projects (:color blue :hint nil :inherit (hydra-common/heads))
  "
                                                                       ╭────────────┐
       Files             Search          Buffer             Do         │ Projectile │
     ╭─────────────────────────────────────────────────────────────────┴────────────╯
       [_f_] file          [_sa_] ag          [_b_] switch         [_g_] magit
       [_l_] file dwim     [_sr_] rg          [_v_] show all       [_p_] switch
       [_r_] recent file   [_so_] occur       [_V_] ibuffer        [_P_] commander
       [_d_] dir           [_sR_] replace     [_K_] kill all       [_i_] info
       [_o_] other         [_st_] find tag
       [_u_] test file     [_sT_] make tags
                                                                           ╭────────┐
       Other Window      Run             Cache              Do             │ Fixmee │
     ╭──────────────────────────────────────────────────╯ ╭────────────────┴────────╯
       [_F_] file          [_U_] test        [_kc_] clear         [_x_] TODO & FIXME
       [_L_] dwim          [_m_] compile     [_kk_] add current   [_X_] toggle
       [_D_] dir           [_c_] shell       [_ks_] cleanup
       [_O_] other         [_C_] command     [_kd_] remove
       [_B_] buffer
     --------------------------------------------------------------------------------
           "

  ("p"   projectile-switch-project)
  ("sa"  counsel-projectile-ag)
  ("sr"  counsel-projectile-rg)
  ("b"   counsel-projectile-switch-to-buffer)
  ("B"   counsel-projectile-switch-to-buffer-other-window)
  ("d"   counsel-projectile-find-dir)
  ("D"   counsel-projectile-find-dir-other-window)
  ("f"   counsel-projectile-find-file)
  ("F"   counsel-projectile-find-file-other-window)
  ("l"   counsel-projectile-find-file-dwim)
  ("L"   counsel-projectile-find-file-dwim-other-window)

  ("c"   projectile-run-async-shell-command-in-root)
  ("C"   projectile-run-command-in-root)
  ("g"   hydra-git/body nil)
  ("i"   projectile-project-info)
  ("kc"  projectile-invalidate-cache)
  ("kd"  projectile-remove-known-project)
  ("kk"  projectile-cache-current-file)
  ("K"   projectile-kill-buffers)
  ("ks"  projectile-cleanup-known-projects)
  ("m"   projectile-compile-project)
  ("o"   projectile-find-other-file)
  ("O"   projectile-find-other-file-other-window)
  ("P"   projectile-commander)
  ("r"   projectile-recentf)
  ("so"   projectile-multi-occur)
  ("sR"   projectile-replace)
  ("st"   projectile-find-tag)
  ("sT"   projectile-regenerate-tags)
  ("u"   projectile-find-test-file)
  ("U"   projectile-test-project)
  ("v"   projectile-display-buffer)
  ("V"   projectile-ibuffer)

  ("X"   fixmee-mode)
  ("x"   fixmee-view-listing))

(define-key projectile-mode-map (kbd "C-c o") #'hydra-project/body)

(defhydra hydra-torus (:color red :hint nil :inherit (hydra-common/heads))

  "
                                                                                             ╭─────────┐
      Switch                    Move                        Do                               │  Torus  │
  ╭──────────────────────────────────────────────────────────────────────────────────────────┴─────────╯
    [_c_] circle          [_<up>_]  prev-location         [_#_]  layout            [_s_] search
    [_l_] location        [_<down>_] next-location        [_ac_] add-circle        [_h_] search-history
    [_t_] torus                                         [_al_] add-location      [_m_] meta-history
                        [_<left>_]  prev-circle         [_at_] add-torus
                        [_<right>_] next-circle
                                                      [_dc_] delete-circle
                                                      [_dl_] delete-location
                        [_<prior>_] newer-history       [_dt_] delete-torus
                        [_<next>_]  older-history
     ^ ^
  -------------------------------------------------------------------------------------------------------
      "

  ("#" torus-layout-menu :color blue)

  ("c" torus-switch-circle :color blue)
  ("l" torus-switch-location :color blue)
  ("t" torus-switch-torus :color blue)

  ("ac" torus-add-circle :color blue)
  ("al" torus-add-location :color blue)
  ("at" torus-add-torus :color blue)

  ("dl" torus-delete-location :color blue)
  ("dc" torus-delete-circle :color blue)
  ("dt" torus-delete-torus :color blue)

  ("<up>"   torus-previous-location)
  ("<down>" torus-next-location)
  ("<left>" torus-previous-circle)
  ("<right>" torus-next-circle)


  ("s" torus-search :color blue)
  ("h" torus-search-history :color blue)
  ("m" torus-search-meta-history :color blue)

  ("<prior>" torus-history-newer)
  ("<next>" torus-history-older)

  )

(defhydra hydra-applications (:exit t :columns 4)
  "Applications"
  ("p" counsel-list-processes "Show Processes")
  ("r" elfeed "RSS Feeds"))

(defvar org-default-projects-dir   "~/org/projects/"                     "Primary GTD directory")
(defvar org-default-notes-dir "~/org/notes/"                     "Directory of notes modeled after Zettelkasten includes an Archive, and Notes")
(defvar org-default-completed-dir  "~/org/projects/completed"            "Directory of completed project files")
(defvar org-default-inbox-file     "~/org/agenda/inbox.org"         "New stuff collects in this file")
(defvar org-default-tasks-file     "~/org/agenda/tasks.org"           "Tasks, TODOs and little projects")
(defvar org-default-incubate-file  "~/org/agenda/incubate.org"        "Ideas simmering on back burner")
(defvar org-default-calendar-file  "~/org/agenda/calendar.org"        "Ideas simmering on back burner")
(defvar org-default-delegate-file  "~/org/agenda/delegate.org"        "Ideas simmering on back burner")
(defvar org-default-waiting-file  "~/org/agenda/waiting.org"        "Ideas simmering on back burner")
(defvar org-default-completed-file nil                              "Ideas simmering on back burner")
(defvar org-default-notes-file     "~/org/agenda/inbox.org"   "Non-actionable, personal notes")

(defhydra hydra-org-refiler (:hint nil)
  "
     ^Update^        ^Refile^         ^Calendar^        ^Ref^         ^Move^           ^Go To^
     ^^^^^^^^^^------------------------------------------------------------------------------------------
     _t_: todo      _T_: tasks      _c c_: calendar  _r d_: web      _m p_: projects   _g p_: projects
     _s_: schedule  _I_: incubate   _c t_: tickler   _r w_: docs     _m n_: notes      _g c_: completed
     _d_: deadline  _W_: waiting    _c d_: delegate  _r i_: images
     _h_: headline  _R_: refile


     "
  ("<up>" org-previous-visible-heading)
  ("<down>" org-next-visible-heading)
  ("k" org-previous-visible-heading)
  ("j" org-next-visible-heading)

  ("T" org-refile-to-task)
  ("I" org-refile-to-incubate)
  ("W" org-refile-to-waiting)
  ("R"  org-refile)

  ("c c" refile-to-calendar)
  ("c t" refile-to-tickler)
  ("c d" org-refile-to-delegate)

  ("r d" note-to-documents)
  ("r w" note-to-websites)
  ("r i" note-to-images)

  ("m p" org-refile-to-projects-dir)
  ("m n" org-refile-to-notes-dir)

  ("t" org-todo)
  ("s" org-schedule)
  ("d" org-deadline)
  ("h" org-rename-header)

  ("g p" (dired org-default-projects-dir))
  ("g c" (dired org-default-completed-dir))
  ("[\t]" (org-cyce))

  ("s" (org-save-all-org-buffers) "save")

  ("a" org-archive-subtree "archive")
  ("d" org-cut-subtree "delete")
  ("q" (my/switch-to-last-buffer) "quit" :color blue))

(defhydra hydra-minor (:exit t :columns 4)
  "Minor modes"
  ("r" spray-mode "Speed read")
  ("n" em/narrow-or-widen-dwim "Narrow / Widen")
  ("i" iedit-mode "Iedit mode"))

(with-no-warnings
  (defhydra hydra-python (:exit t :columns 4
                                :inherit (hydra-common/heads))
    "Python"
    ("#" poporg-dwim "Edit Comment")
    ("," dumb-jump-back "Jump back")
    ("." dumb-jump-go "Jump to definition")
    ("D" hydra-python-django/body "Django")
    ("L" flycheck-prev-error "Prev lint error")
    ("T" pythonic-tests-all "Run pythonic test")
    ("a" pytest-all "Run all tests")
    ("b" python-shell-send-buffer "Send buffer to python")
    ("u" em-python-pur "Upgrade pip requirements")
    ;;("r" python-shell-send-buffer "Send line/region to python")
    ("r" run-python "REPL")
                                        ;    ("d" helm-dash-at-point "Docs")
    ("c" em-python-execute "Compile / Execute")
    ("i" dumb-jump-quick-look "Definition Info")
    ("l" flycheck-next-error "Next lint error")
    ("t" pythonic-tests-run "Run current test")
    ("V" flycheck-verify-setup "Verify linting")
    ("v" em-python-environment "Check environment")
    ("s" isortify-buffer "Sort imports")
    ;;("n" flyceck "Run all tests")
    ("f" blacken-buffer "Format buffer code")))

(defhydra hydra-exwm (:exit t
                            :columns 2
                            :color blue
                            :inherit (hydra-common/heads))

  "EXWM"
  ("0" delete-window "delete")

  ("s"  #'exwm-input-send-next-key "send key")

  ("1" ace-delete-window "other")

  ("t"  #'exwm-floating-toggle-floating "toggle floating")

  ("2" split-window-below "below")

  ("f"  #'exwm-layout-set-fullscreen "fullscreen")

  ("3" split-window-right "right")

  ("m"  #'exwm-workspace-move-window "move window")

  (";" ace-swap-window "swap")

  ("r" rename-buffer "rename")

  ("e" exwm-edit--compose "compose")

  ("g" hycontrol-windows-grid "grid"))

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
   ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
  -^----------^-+-^----^--------+-^-------^--------+-^----^-------
    _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
   _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
    _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
  -^----------^-+-^----^--------+-^-------^--------+-^----^-------
  "
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" ibuffer-quit "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                                     :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                       :after-exit
                                       (if (eq major-mode 'ibuffer-mode)
                                           (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-org-table-helper (:color pink :hint nil)
  "
  org table helper
  _r_ recalculate     _w_ wrap region      _c_ toggle coordinates
  _i_ iterate table   _t_ transpose        _D_ toggle debugger
  _B_ iterate buffer  _E_ export table     _n_ remove number separators
  _e_ eval formula    _s_ sort lines       _d_ edit field

  _q_ quit
  "
  ("E" org-table-export :color blue)
  ("s" org-table-sort-lines)
  ("d" org-table-edit-field)
  ("e" org-table-eval-formula)
  ("r" org-table-recalculate)
  ("i" org-table-iterate)
  ("B" org-table-iterate-buffer-tables)
  ("w" org-table-wrap-region)
  ("D" org-table-toggle-formula-debugger)
  ("t" org-table-transpose-table-at-point)
  ("n" dfeich/org-table-remove-num-sep :color blue)
  ("c" org-table-toggle-coordinate-overlays :color blue)
  ("q" nil :color blue))

(defhydra hydra-babel-helper (:color pink :hint nil)
  "
  org babel src block helper functions
  _n_ next       _i_ info           _I_ insert header
  _p_ prev       _c_ check
  _h_ goto head  _E_ expand
  ^ ^            _s_ split
  _q_ quit       _r_ remove result  _e_ examplify region

  "
  ("i" org-babel-view-src-block-info)
  ("I" org-babel-insert-header-arg)
  ("c" org-babel-check-src-block :color blue)
  ("s" org-babel-demarcate-block :color blue)
  ("n" org-babel-next-src-block)
  ("p" org-babel-previous-src-block)
  ("E" org-babel-expand-src-block :color blue)
  ("e" org-babel-examplify-region :color blue)
  ("r" org-babel-remove-result :color blue)
  ("h" org-babel-goto-src-block-head)
  ("q" nil :color blue))

(defun context-hydra-launcher ()
  "A launcher for hydras based on the current context."
  (interactive)
  (cl-case major-mode
    ('org-mode (let* ((elem (org-element-context))
                      (etype (car elem))
                      (type (org-element-property :type elem)))
                 (cl-case etype
                   (src-block (hydra-babel-helper/body))
                   (link (hydra-org-link-helper/body))
                   ((table-row table-cell) (hydra-org-table-helper/body))
                   (t (message "No specific hydra for %s/%s" etype type)
                      (hydra-org-default/body)))))

    ('bibtex-mode (org-ref-bibtex-hydra/body))
    ('ibuffer-mode (hydra-ibuffer-main/body))

    (t (message "No hydra for this major mode: %s" major-mode))))

(defhydra hydra-org-link (:color pink :hint nil)
  "
  org link helper
  _i_ backward slurp     _o_ forward slurp    _n_ next link
  _j_ backward barf      _k_ forward barf     _p_ previous link

  _q_ quit
  "
  ("i" org-link-edit-backward-slurp)
  ("o" org-link-edit-forward-slurp)
  ("j" org-link-edit-backward-barf)
  ("k" org-link-edit-forward-barf)
  ("n" org-next-link)
  ("p" org-previous-link)
  ("q" nil :color blue))

(defhydra hydra-clock (:color blue)
  "
      ^
      ^Clock^             ^Do^
      ^─────^─────────────^──^─────────
      _q_ quit            _c_ cancel
      ^^                  _d_ display
      ^^                  _e_ effort
      ^^                  _i_ in
      ^^                  _g_ goto
      ^^                  _o_ out
      ^^                  _r_ report
      ^^                  ^^
      "
  ("q" nil)
  ("c" org-clock-cancel :color pink)
  ("d" org-clock-display)
  ("e" org-clock-modify-effort-estimate)
  ("i" org-clock-in)
  ("g" org-clock-goto)
  ("o" org-clock-out)
  ("r" org-clock-report))

(defhydra windows-hydra ()
  "Window Management"
  ("a" (call-interactively #'ace-window) "ace")
  ("v" (flip-frame) "flip-vertically")
  ("h" (flop-frame) "flop-horizontally")
  ("r" (rotate-frame-clockwise) "rotate clockwise")
  ("R" (rotate-frame-anticlockwise) "rotate anti-clockwise")
  ("t" (transpose-frame) "transpose")
  ("w" (call-interactively #'exwm-workspace-move-window) "exwm move win to workspace")
  ("<left>" (call-interactively #'shrink-window-horizontally) "shrink-window-horizontally")
  ("<right>" (call-interactively #'enlarge-window-horizontally) "enlarge-window-horizontally")
  ("<down>" (call-interactively #'shrink-window) "shrink-window")
  ("<up>" (call-interactively #'enlarge-window) "enlarge-window")
  ("<s-up>" (windmove-up) "move up")
  ("<s-down>" (windmove-down) "move down")
  ("<s-right>" (windmove-right) "move right")
  ("<s-left>" (windmove-left) "move left")
  ("0" (delete-window) "")
  ("s-0" (delete-window) "")
  ("1" (delete-other-windows) "")
  ("s-1" (delete-other-windows) "")
  ("2" (split-window-below) "")
  ("s-2" (split-window-below) "")
  ("3" (split-window-right) "")
  ("s-3" (split-window-right) "")
  ("q" nil "Quit"))

(defhydra hydra-caps (:exit nil)
  "NAVI-MODE"
  ("<menu>" nil)

  ("=" ((lambda ()
          (start-process-shell-command "notify-send" nil "notify-send Smarparens-Strict-Mode Toggled")
          (call-interactively #'smartparens-strict-mode))))

  ("C-g" (keyboard-quit))
  ("g" (keyboard-quit))
  ("SPC" (call-interactively #'set-mark-command))
  ("C-SPC" (call-interactively #'set-mark-command))
  ("C-n" (next-line))
  (";" (call-interactively #'goto-last-change))
  ("," (call-interactively #'goto-last-change-reverse))
  ("C-p" (previous-line))
  ("C-f" (forward-char))
  ("C-b" (backward-char))
  ("C-v" (scroll-up-command))
  ("v" (scroll-up-command))
  ("M-v" (scroll-down-command))
  ("V" (scroll-down-command))

  ("j" (dumb-jump-go))
  ("k" (dumb-jump-back))
  ("l" (dumb-jump-quick-look))

  ("<" (beginning-of-buffer))
  (">" (end-of-buffer))
  ("\[" (backward-paragraph))
  ("\]" (forward-paragraph))
  ("s-f" (sp-forward-symbol))
  ("M-f" (forward-word))
  ("s-b" (sp-backward-symbol))
  ("M-b" (backward-word))

  ("e" (sp-forward-sexp))
  ("a" (sp-backward-sexp))
  ("f" (sp-down-sexp))
  ("b" (sp-up-sexp))
  ("m" (call-interactively #'magit-status))
  ("n" (sp-next-sexp))
  ("p" (sp-previous-sexp))
  ("s" (sp-select-next-thing))
  ("S" (sp-select-previous-thing))

  ("B" (helm-buffers-list))
  ("E" ((lambda ()
          (flycheck-mode 1)
          (flycheck-list-errors))))
  ("P" (projectile-commander))
  ("F" (call-interactively #'helm-find-files))
  ("D" (dired (helm-current-directory)))
  ("M" (call-interactively #'magit-status))

  ("/" (helm-swoop))
  ("+" (operate-on-number-at-point)))

(defhydra hydra-undo-tree (:color yellow
                                  :hint nil
                                  )
  "
    _p_: undo  _n_: redo _s_: save _l_: load   "
  ("p"   undo-tree-undo)
  ("n"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("u"   undo-tree-visualize "visualize" :color blue)
  ("q"   nil "quit" :color blue))

(defhydra hydra-pdftools (:color blue :hint nil)
  "
                                                                        ╭───────────┐
         Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
     ╭──────────────────────────────────────────────────────────────────┴───────────╯
        ^^^_g_^^^       _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search      [_u_] revert buffer
        ^^^^↑^^^^       ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline     [_i_] info
        ^^^_p_^^^       ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link        [_d_] midgnight mode
        ^^^^↑^^^^       ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link [_D_] print mode
   _h_ ← _e_/_t_ → _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
        ^^^^↓^^^^       ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
        ^^^_n_^^^       ^ ^  _r_eset slice box
        ^^^^↓^^^^
        ^^^_G_^^^
     --------------------------------------------------------------------------------
          "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y"  pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :color red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-midnight-minor-mode)
  ("D" pdf-view-printer-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("g" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("t" pdf-view-goto-label)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red))
