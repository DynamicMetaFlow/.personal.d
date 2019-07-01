(use-package pdf-tools
  :defer 0.1
  :config
  (unless noninteractive
    (pdf-tools-install))
  (setq-default pdf-view-display-size 'fit-page))
