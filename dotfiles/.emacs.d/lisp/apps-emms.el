;;; -*- lexical-binding: t; -*-

(use-package emms
  :config
  (when (file-directory-p "~/Downloads")
    (setq emms-source-file-default-directory "~/Downloads"))
  (emms-all)
  (emms-default-players)
  (emms-playing-time -1)
  (emms-mode-line -1))

(setq emms-stream-default-list
      (append emms-stream-default-list
              '(("SomaFM: Defcon" "https://somafm.com/defcon.pls" 1 streamlist)
                ("SomaFM: SF10-33" "https://somafm.com/sf1033.pls" 1 streamlist)
                ("SomaFM: Dub Step Beyond" "https://somafm.com/dubstep.pls" 1 streamlist)
                ("SomaFM: Illinois Street Lounge" "https://somafm.com/illstreet.pls" 1 streamlist))))

(provide 'apps-emms)
