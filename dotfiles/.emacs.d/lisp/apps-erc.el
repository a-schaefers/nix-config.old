;;; -*- lexical-binding: t; -*-

(require 'erc)
(use-package erc-image
  :after erc
  :config
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

(defun erc-connect ()
  "Connect to an irc server using creds in .authinfo or .authinfo.gpg."
  (interactive)
  (setq erc-autojoin-timing "ident"
        erc-prompt-for-password nil
        erc-nick "adamantium"
        erc-autojoin-channels-alist '(("freenode.net"
                                       "#emacs"
                                       "#nixos"
                                       )))
  (erc-tls :server "chat.freenode.net" :port "6697"))

;; hide join/part/quit messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; friends list
(setq erc-pals '("bayprogrammer"))

;; show only mentions and pals on my modeline
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
(setq erc-track-exclude '("chat.freenode.net:6697"
                          "freenode.net"))

;; "bury" private message buffers, but notify also on the modeline.
(setq erc-auto-query 'bury)
(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))
(defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
  (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'nil))
  ad-do-it
  (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'all)))

;; Notify me when someone mentions my nick or aliases on IRC.
(defun erc-global-notify (matched-type nick msg)
  (interactive)
  (when (or (eq matched-type 'current-nick) (eq matched-type 'keyword)))
  (if (string-match "^adamantium.[-_A-Za-z0-9 ]+$" msg)
      (shell-command
       (concat "notify-send '" nick " "
               (replace-regexp-in-string "^adamantium." "" msg) "'"))))
(add-hook 'erc-text-matched-hook 'erc-global-notify)

;; better flood protection! PRO TIP: use a pastebin!
(defun my-erc-multi-line-disable (string)
  "disable multi-line erc messages"
  (if (string-match-p "\n+" string)
      (setq str nil)))
(add-hook 'erc-send-pre-hook 'my-erc-multi-line-disable)

(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

(provide 'apps-erc)
