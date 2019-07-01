(after "general-autoloads"

(general-define-key
"C-x b"    #'ivy-switch-buffer-non-exwm
"<print>"  #'desktop-environment-screenshot
"C-4"      #'my/org-capture-appt
"C-1"      #'my/org-capture-task
"C-2"      #'my/org-capture-journal
"C-3"      #'my/org-capture-note
"s-p"      #'my/switch-to-last-buffer
"s-SPC"    #'exwm-jump-to-last-exwm
"s-<tab>"  #'ivy-switch-buffer-exwm
"s-f"      #'toggle-single-window
"s-,"      #'winner-undo
"s-."      #'winner-redo
"s-r"      #'exwm-reset
"s-w"      #'exwm-workspace-switch
"s-l"      #'windmove-right
"s-k"      #'windmove-left
"s-i"      #'windmove-up
"s-o"      #'windmove-down
"s-L"      #'buf-move-right
"s-K"      #'buf-move-left
"s-I"      #'buf-move-up
"s-O"      #'buf-move-down
"s-x"      #'exwm-input-toggle-keyboard)

    (after "org-contrib-autoloads"

      (general-def org-mode-map


      )


      (general-def org-agenda-mode-map
      "x"   'org-agenda-exit
      "'"   'org-agenda-set-restriction-lock-from-agenda
      "\\"   'org-agenda-remove-restriction-lock

      )
)

  (general-define-key
  :prefix "C-c"

  )

    (general-define-key
         :keymaps 'key-translation-map
         "ESC" (kbd "C-g"))

        (defconst my-leader "C-c")

        (general-create-definer my-leader-def
          :prefix my-leader)

    ;; Global
        (my-leader-def



          )

    ;; Modes
        (my-leader-def
          :keymaps 'org-mode-map

          )

    (general-unbind
      "C-x m"
      )

)
