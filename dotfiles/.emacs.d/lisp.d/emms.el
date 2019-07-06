;;; -*- lexical-binding: t; -*-

;;pdf-tools breaks without package
(require 'package)
(package-initialize)
(require 'pdf-tools)
(pdf-tools-install t)

(require 'emms-setup)
(emms-standard)
(emms-default-players)

(when (file-directory-p "~/Downloads")
  (setq emms-source-file-default-directory "~/Downloads"))

(setq emms-player-list '(emms-player-vlc emms-player-vlc-playlist))

(emms-playing-time -1)
(emms-mode-line -1)
