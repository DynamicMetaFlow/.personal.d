(use-package circe
  :config
  (require 'circe)
  (setf (cdr tracking-mode-map) nil)
  (defun my/rename-irc-channel-buffer ()
    (rename-buffer (concat (buffer-name) "@" (with-circe-server-buffer circe-network))))
  (add-hook 'circe-channel-mode-hook 'my/rename-irc-channel-buffer)

  (defun my/highlight-channel ()
    (interactive)
    (setq-local my/buffer-notify t))
  (add-hook 'tracking-buffer-added-hook 'my/highlight-channel)
  (defun my/de-highlight-channel ()
    (interactive)
    (setq-local my/buffer-notify nil))
  (add-hook 'tracking-buffer-removed-hook 'my/de-highlight-channel)
  (enable-circe-color-nicks)
  (setq lui-fill-column 100000
        lui-time-stamp-position 'left
        circe-lagmon-timer-tick 120
        circe-reduce-lurker-spam t
        circe-server-buffer-name "{network}"
        circe-default-nick "Dynamicmetaflow"
        circe-default-user "Dynamicmetaflow"
        circe-default-realname "Dynamicmetaflow"
        ))
