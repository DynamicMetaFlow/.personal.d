(after "general-autoloads"

      (general-define-key
;      "C-x b"     'ivy-switch-buffer-non-exwm

)

    (after "org-contrib-autoloads"

      (general-def org-mode-map


      )


      (general-def org-agenda-mode-map
      "x"   'org-agenda-exit

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
