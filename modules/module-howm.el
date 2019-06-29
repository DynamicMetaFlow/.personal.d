(use-package howm
:config

  (defvar howm-view-title-header "#+TITLE:")

  (defvar howm-view-header-format
    "\n\n#+INCLUDE: %s\n")

  (setq howm-template-rules
        '(("%title" . howm-template-title)
          ("%date" . howm-template-date)
          ("%file" . howm-template-previous-file)
          ("%parent" . howm-template-parent)
          ("%fname" . howm-template-filename)
          ("%cursor" . howm-template-cursor)))

  (defun howm-template-title (arg)
    (insert (cdr (assoc 'title arg))))

  (defun howm-template-filename (arg)
    (insert (concat ">>>" (file-name-base buffer-file-name))))

  (defun howm-template-parent (arg)
    (insert (cdr (assoc 'parent arg))))

  (defun howm-template-date (arg)
    (insert (cdr (assoc 'date arg))))

  (defun howm-template-previous-file (arg)
    (insert (cdr (assoc 'file arg))))

  (defun howm-template-cursor (arg))

  (setq howm-file-name-format "%Y-%m-%dT%H.%M.%S.org")
  (setq howm-template-date-format "#+DATE: [%Y-%m-%d %H:%M]")
  (setq howm-directory "~/org/notes/")
  (setq howm-view-preview-narrow nil)

  (add-hook 'org-mode-hook 'howm-mode)
  (add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))



  (setq howm-view-split-horizontally t)
  (setq howm-view-keep-one-window t)

  (setq howm-menu-refresh-after-save nil)
  (setq howm-menu-expiry-hours 6)  ;; cache menu N hours
  (setq howm-menu-file "0000-00-00-000000.txt")  ;; don't *search*

  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)

  ;; howm-menu
  (defun howm-menu-with-j1 (orig-fun &rest args)
    (setq howm-view-grep-option "-nH --no-heading -j1 --color never")
    (apply orig-fun args)
    (setq howm-view-grep-option "-nH --no-heading --color never"))

  (advice-add 'howm-menu-refresh :around #'howm-menu-with-j1)

  (setq howm-view-search-in-result-correctly t)

  (setq howm-view-list-title-type 2)
  (setq howm-view-summary-format "")

  (defun howm-search-title (title)
    (interactive "sSearch title: ")
    (message title)
    (howm-search (format "^* +%s" (regexp-quote title)) nil))

  (defun howm-list-grep-in-new-frame (&optional completion-p)
    (interactive "P")
    (select-frame (make-frame))
    (howm-list-grep completion-p))

  (defvar *howm-new-frame* nil)

  (defun howm-new-frame ()
    (when *howm-new-frame*
      (select-frame (make-frame))))
  (add-hook 'howm-view-before-open-hook 'howm-new-frame)

  (defun howm-open-new-frame (opener)
    ;; move cursor back from contents to summary in the original frame
    (let (new-frame)
      (save-window-excursion
        (let ((*howm-new-frame* t))
          (funcall opener))
        (setq new-frame (selected-frame)))
      (select-frame new-frame)))

  (defun howm-open-new-frame-summary ()
    (interactive)
    (howm-open-new-frame #'howm-view-summary-open-sub))

  (defun howm-open-new-frame-contents ()
    (interactive)
    (howm-open-new-frame #'howm-view-contents-open-sub))


  (defun howm-create-and-link (&optional which-template)
    (interactive "p")
    (let ((b (current-buffer))
          (p (point)))
      (prog1
          (howm-create which-template)
        (let ((f (buffer-file-name)))
          (when (and f (buffer-file-name b))
            (with-current-buffer b
              (goto-char p)
              (insert (format howm-template-file-format
                              (abbreviate-file-name f))
                      "\n")))))))

  (defun howm-open-from-calendar ()
    (interactive)
    (require 'howm-mode)
    (let* ((mdy (calendar-cursor-to-date t))
           (m (car mdy))
           (d (second mdy))
           (y (third mdy))
           (ti (encode-time 0 0 0 d m y))
           (pc (howm-folder-get-page-create howm-directory (howm-file-name ti)))
           (page (car pc))
           (createp (cdr pc)))
      (other-window 1)
      (howm-page-open page)
      (if createp
          (howm-create-here)
        (howm-set-mode))))
  (require 'calendar)


  (defun my-howm-switch-to-summary ()
    (interactive)
    (switch-to-buffer "*howmS*")
    (riffle-summary-check t))

  (add-hook 'howm-view-contents-mode-hook
            (lambda ()
              (setq default-directory howm-directory)
              (howm-mode 1)))
  (defadvice riffle-contents-show (around howm-mode (item-list) activate)
    ad-do-it
    (when howm-mode
      (howm-initialize-buffer)))


  (defun howm-export-to-org ()
    "Remove formatting and export to plain text
    when in howmC view"
    (interactive)
    (copy-whole-buffer-to-clipboard)
    (find-file   (concat "~/notes_export_" (format-time-string "%m-%d-%H%M%S") ".org"))
    (yank)
    (goto-char(point-min))
    (replace-string  "#+TITLE: "  "* ")
    (goto-char(point-min))
    (replace-string "#+DATE: " "")
    (goto-char(point-min))
    (replace-string "#+KEYWORDS: " "")
    (goto-char(point-min))
    (replace-regexp "^==========================>>> .*$" ""))


  (defun howm-insert-filename ()
    (interactive)
    (insert (concat ">>>" (file-name-base buffer-file-name))))


  (defun howm-create (&optional which-template here)
    (interactive "p")
    (let* ((t-c (howm-create-default-title-content))
           (title (car t-c))
           (content (cdr t-c)))
      (howm-create-file-with-title title which-template nil here content)
      (org-cycle '(16))
      ))


  (defun my/howm-view-summary-open ()
    (interactive)
    (howm-view-summary-open)
    (delete-other-windows))

  (defun my/howm-view-summary-open ()
    (interactive)
    (howm-view-summary-open)
    (org-cycle '(16)))

  (defun howm-org-include-file ()
    (interactive)
    (save-excursion
      (setq current-buffer buffer-file-name)
      (switch-to-buffer "*scratch*")
      (insert "\n\n")
      (insert "#+INCLUDE: \"" current-buffer "\" :only-contents t :lines \"10-\"\n\n")))

 (require 'howm)
(setq howm-template
  ":HIDDEN:
  #+PARENTS:
  #+CHILDREN:
  #+FRIENDS:

  :RELATED:
  %file
  %title

  :RESOURCES:

  :END:
  #+TITLE: %cursor
  %fname
  %date
  #+CATEGORY:
  #+KEYWORDS:

    ")

    (define-key howm-view-summary-mode-map (kbd "M-C-m") 'howm-open-new-frame-summary)
    (define-key howm-view-summary-mode-map [tab] 'my/howm-view-summary-open)
)
