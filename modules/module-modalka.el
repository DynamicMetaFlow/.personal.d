(use-package modalka
  :config
  (modalka-global-mode 1)

  (defun normal-mode-modalka ()
    (interactive)
    (if (modalka-mode nil)
        (modalka-mode 1)
      (nil)))

  (defun insert-mode-modalka ()
    (interactive)
    (modalka-mode 0))

  (setq-default cursor-type '(bar . 1))
  (setq modalka-cursor-type 'box)

  (setq org-capture-mode-hook 'insert-mode-modalka)

(defun modalka-select-major-mode (modalka-mode-map)
  (let ((modalka-mode-command (cdr (assoc major-mode modalka-mode-map))))
    (if modalka-mode-command (apply modalka-mode-command))))

(defun modalka-mode-hydra ()
  (interactive)
  (modalka-select-major-mode modalka-major-mode-hydra-list))

(setq modalka-key-list '
      ((org-mode . (message "%s" "command from org mode"))
       (lisp-mode . (message "%s" "command from lisp mode"))
       (python-mode . (message "%s" "command from python mode"))))

(setq modalka-major-mode-hydra-list '
      ((org-mode . (matcha-org-mode))
       (emacs-lisp-mode . (matcha-emacs-lisp-mode))
       (js-mode . (matcha-indium-mode/body))
       (js2-mode . (matcha-indium-mode/body))
       (rjsx-mode . (matcha-indium-mode/body))
       (json-mode . (matcha-json-mode))
       (css-mode . (matcha-css-mode))
       (scss-mode . (matcha-css-mode))
       (web-mode . (matcha-web-mode/body))
       (html-mode . (matcha-web-mode/body))
       (mhtml-mode . (matcha-web-mode/body))
       (exwm-mode . (hydra-exwm/body))
       (elfeed-search-mode . (hydra-elfeed-search/body))
       (python-mode . (matcha-python-mode))))


(custom-set-variables
 '(modalka-excluded-modes
   (quote
    (
     ediff-mode
     helpful-mode
     guix-mode
     dired-mode
     magit-mode
     magit-popup-mode
     debugger-mode
     ediff-mode
     help-mode
     git-rebase-mode
     mu4e-headers-mode
     mu4e-view-mode
     help-mode
     org-agenda-mode
     org-capture-mode
     emms-playlist-mode
     pdf-tools-modes
     *dashboard*
     *Messages*
     makey-key-mode
     ))))

(define-key modalka-mode-map [remap self-insert-command] 'ignore)

(define-key global-map [escape] #'normal-mode-modalka)
(define-key modalka-mode-map (kbd "SPC") 'counsel-M-x)
(define-key modalka-mode-map (kbd "<return>") 'counsel-linux-app)
(define-key modalka-mode-map (kbd "<tab>") 'my/switch-to-last-buffer)


(define-key modalka-mode-map (kbd "a")  'matcha-org-agenda)
(define-key modalka-mode-map (kbd "b")  'ivy-switch-buffer)
(define-key modalka-mode-map (kbd "c")  'hydra-copy/body)
(define-key modalka-mode-map (kbd "d")  'dired-jump)
(define-key modalka-mode-map (kbd "e")  'hydra-emacs/body)
(define-key modalka-mode-map (kbd "f")  'hydra-file/body)
(define-key modalka-mode-map (kbd "g")  'magit-status)
(define-key modalka-mode-map (kbd "h")  'hydra-help/body)
(define-key modalka-mode-map (kbd "i") #'insert-mode-modalka)
(define-key modalka-mode-map (kbd "j")  'hydra-jump/body)
(define-key modalka-mode-map (kbd "k")  'hydra-delete/body)
(define-key modalka-mode-map (kbd "l")  'org-goto-current-datetree-entry)
(define-key modalka-mode-map (kbd "m")  'modalka-mode-hydra)
(define-key modalka-mode-map (kbd "n")  'hydra-narrow/body)
(define-key modalka-mode-map (kbd "o")  'objed-activate)
(define-key modalka-mode-map (kbd "p")  nil)
(define-key modalka-mode-map (kbd "q")  'hydra-master/body)
(define-key modalka-mode-map (kbd "r")  'hydra-register/body)
(define-key modalka-mode-map (kbd "s")  nil)
(define-key modalka-mode-map (kbd "t")  'torus-search)
(define-key modalka-mode-map (kbd "u")  nil)
(define-key modalka-mode-map (kbd "v")  nil)
(define-key modalka-mode-map (kbd "w")  'hydra-window/body)
(define-key modalka-mode-map (kbd "x")  nil)
(define-key modalka-mode-map (kbd "y")  'hydra-paste/body)
(define-key modalka-mode-map (kbd "z")  'ivy-resume)

(define-key modalka-mode-map (kbd "A")  nil)
(define-key modalka-mode-map (kbd "B")  'ivy-switch-buffer-exwm)
(define-key modalka-mode-map (kbd "C")  nil)
(define-key modalka-mode-map (kbd "D")  nil)
(define-key modalka-mode-map (kbd "E")  nil)
(define-key modalka-mode-map (kbd "F")  nil)
(define-key modalka-mode-map (kbd "G")  'hydra-git/body)
(define-key modalka-mode-map (kbd "H")  nil)
(define-key modalka-mode-map (kbd "I")  nil)
(define-key modalka-mode-map (kbd "J")  nil)
(define-key modalka-mode-map (kbd "K")  nil)
(define-key modalka-mode-map (kbd "L")  nil)
(define-key modalka-mode-map (kbd "M")  'hydra-minor/body)
(define-key modalka-mode-map (kbd "N")  nil)
(define-key modalka-mode-map (kbd "O")  nil)
(define-key modalka-mode-map (kbd "P")  nil)
(define-key modalka-mode-map (kbd "Q")  nil)
(define-key modalka-mode-map (kbd "R")  nil)
(define-key modalka-mode-map (kbd "S")  nil)
(define-key modalka-mode-map (kbd "T")  'hydra-torus/body)
(define-key modalka-mode-map (kbd "U")  nil)
(define-key modalka-mode-map (kbd "V")  nil)
(define-key modalka-mode-map (kbd "W")  nil)
(define-key modalka-mode-map (kbd "X")  nil)
(define-key modalka-mode-map (kbd "Y")  nil)
(define-key modalka-mode-map (kbd "Z")  nil)

(define-key modalka-mode-map (kbd "0")  'delete-window)
(define-key modalka-mode-map (kbd "1")  'delete-other-windows)
(define-key modalka-mode-map (kbd "2")  'split-window-below)
(define-key modalka-mode-map (kbd "3")  'split-window-right)
(define-key modalka-mode-map (kbd "4")  nil)
(define-key modalka-mode-map (kbd "5")  nil)
(define-key modalka-mode-map (kbd "6")  nil)
(define-key modalka-mode-map (kbd "8")  nil)
(define-key modalka-mode-map (kbd "9")  nil)

)
