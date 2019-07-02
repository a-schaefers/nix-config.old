;;; -*- lexical-binding: t; -*-

(require 'erc)

(defun my-erc ()
  (interactive)
  (setq erc-autojoin-timing "ident"
        erc-prompt-for-password nil
        erc-nick "adamantium"
        erc-autojoin-channels-alist '(("freenode.net"
                                       "#emacs"
                                       "#nixos"
                                       "#funtoo-report"
                                       "#tinfoilhats"
                                       "##apoptosis"
                                       "#commanduser"
                                       )))
  (erc-tls :server "chat.freenode.net" :port "6697"))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(setq erc-pals '("bayprogrammer"))

(setq erc-format-query-as-channel-p t
      erc-track-priority-faces-only 'all
      erc-track-faces-priority-list '(erc-error-face
                                      erc-current-nick-face
                                      erc-keyword-face
                                      erc-nick-msg-face
                                      erc-direct-msg-face
                                      erc-dangerous-host-face
                                      erc-notice-face
                                      erc-prompt-face
                                      erc-pal-face))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-exclude-server-buffer t)
(setq erc-track-exclude '("chat.freenode.net:6697"))

(setq erc-auto-query 'bury)
(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))
(defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
  (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'nil))
  ad-do-it
  (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'all)))

(defun my-erc-multi-line-disable (string)
  "disable sending of multi-line messages"
  (if (string-match-p "\n+" string)
      (setq str nil)))
(add-hook 'erc-send-pre-hook 'my-erc-multi-line-disable)
