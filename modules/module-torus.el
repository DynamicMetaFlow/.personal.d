(use-package torus
  :defer 0.1
  :bind-keymap ("C-x t" . torus-map)
  :bind (
         :map torus-map
         ("t" . torus-copy-to-circle))
  :hook ((emacs-startup . torus-start)
         (kill-emacs . torus-quit))
  :custom (
           (torus-binding-level 1)
           (torus-verbosity 1)
           (torus-dirname (concat user-emacs-directory (file-name-as-directory "torus")))
           (torus-load-on-startup t)
           (torus-save-on-exit t)
           (torus-autoread-file (concat torus-dirname "last.el"))
           (torus-autowrite-file torus-autoread-file)
           (torus-backup-number 5)
           (torus-history-maximum-elements 30)
           (torus-maximum-horizontal-split 3)
           (torus-maximum-vertical-split 4)
           (torus-display-tab-bar t)
           (torus-separator-torus-circle " >> ")
           (torus-separator-circle-location " > ")
           (torus-prefix-separator "/")
           (torus-join-separator " & "))
  :config
  (torus-init)
  (torus-install-default-bindings)
  (defun torus-read (filename)
    "Read main torus variables from FILENAME as Lisp code."
    (interactive
     (list
      (read-file-name
       "Torus file : "
       (file-name-as-directory torus-dirname))))
    (let*
        ((file-basename (file-name-nondirectory filename))
         (minus-len-ext (- (min (length torus-extension)
                                (length filename))))
         (buffer))
      (unless (equal (cl-subseq filename minus-len-ext) torus-extension)
        (setq filename (concat filename torus-extension)))
      (when (torus--update-input-history file-basename)
        (if (file-exists-p filename)
            (progn
              (setq buffer (find-file-noselect filename))
              (eval-buffer buffer)
              (kill-buffer buffer))
          (message "File %s does not exist." filename))))
    ;; Also saved in file
                                        ;(torus--update-meta)
                                        ;(torus--build-index)
                                        ;(torus--build-meta-index)
    (torus--jump))
  (setq torus-prefix-key (kbd "C-x t"))

  )
