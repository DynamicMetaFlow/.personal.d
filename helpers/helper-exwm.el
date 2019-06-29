(defun my/switch-to-last-buffer ()
    "Switch to last open buffer in current window."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

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
