;;pdf-tools breaks without package
(require 'package)
(package-initialize)
(require 'pdf-tools)
(pdf-tools-install t)

(require 'emms)
(require 'emms-setup)
(emms-all)
(emms-default-players)
(emms-playing-time -1)
(emms-mode-line -1)
(when (file-directory-p "~/Downloads")
  (setq emms-source-file-default-directory "~/Downloads"))
