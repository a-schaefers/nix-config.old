;;; -*- lexical-binding: t; -*-

(use-package pdf-tools :defer t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t))

(provide 'apps-pdf-tools)
