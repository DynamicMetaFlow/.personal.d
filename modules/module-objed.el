(use-package objed
  :config

  (require 'objed)

  (add-to-list 'objed-keeper-commands 'undo-tree-undo)
  (add-to-list 'objed-keeper-commands 'undo-tree-redo)
  (add-to-list 'objed-keeper-commands 'undo-tree-visualize)

  (defvar objed--extra-face-remaps nil)

  (defun objed-add-face-remaps (&rest _)
    "Add extra face remaps when objed activates."
    (when (memq 'objed-hl (assq 'hl-line face-remapping-alist))
      (push (face-remap-add-relative 'solaire-hl-line-face 'objed-hl)
            objed--extra-face-remaps)))

  (defun objed-remove-face-remaps (&rest _)
    "Remove extra face remaps when objed de-activates."
    (unless (memq 'objed-hl (assq 'hl-line face-remapping-alist))
      (dolist (remap objed--extra-face-remaps)
        (face-remap-remove-relative remap))
      (setq objed--extra-face-remaps nil)))

  (advice-add 'objed--init :after #'objed-add-face-remaps)
  (advice-add 'objed--reset :after #'objed-remove-face-remaps)

                                        ;(define-key objed-user-map "f" nil)
  (define-key objed-user-map "d" 'xref-find-definitions)
  (define-key objed-user-map "r" 'xref-find-references)

  (define-key objed-op-map "j" 'counsel-imenu)
  (define-key objed-op-map "f" 'counsel-find-file)
  (define-key objed-op-map "b" 'ivy-switch-buffer))
