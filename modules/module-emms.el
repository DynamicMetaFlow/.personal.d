(use-package emms
  :config
  (require 'emms)
  (emms-all)
  (emms-default-players)
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-info-asynchronously t)
  (setq emms-info-functions '(emms-info-libtag))
  (emms-mode-line 0)
  (emms-playing-time 1)

  (setq emms-source-file-default-directory "/home/alexander/org/data/c0/80320c-060b-4348-a413-ee7d8ed40dd6/"))
