;;; -*- lexical-binding: t; -*-

;; put <menu> on <caps lock> and then bind hydra to <menu>

(start-process-shell-command "setxkbmap" nil "setxkbmap -option caps:menu")

(require 'hydra)

(global-set-key (kbd "<menu>") 'caps-hydra/body)

(with-eval-after-load 'exwm ;; in case also using exwm
  (exwm-input-set-key (kbd "<menu>") 'caps-hydra/body))

(defhydra caps-hydra (:exit t)
  "Menu"
  ("#" (my-shell) "sh")
  ("!" (lambda (command)
         (interactive (list (read-shell-command "$ ")))
         (start-process-shell-command command nil command)) "cmd")
  ("c" (abook-hydra/body) "contacts")
  ("e" (gnus) "email")
  ("i" (my-erc) "irc")
  ("b" (call-interactively #'eww) "eww")
  ("m" (emms-hydra/body) "emms")
  ("w" (windows-hydra/body) "win")
  ("<menu>" nil))

(defhydra abook-hydra (:exit t)
  "contacts list"
  ("i" (my-insert-contact) "insert email address")
  ("e" (lambda ()
         (interactive)
         (when (file-exists-p "~/Private/contacts.el")
           (find-file "~/Private/contacts.el"))) "edit contacts"))

(defhydra emms-hydra (:exit t)
  "EMMS"
  ("r" (emms-streams) "radio streams")
  ("f" (call-interactively #'emms-play-file) "open file")
  ("l" (emms) "playlist"))

;; a nested window mgmt hydra
(require 'transpose-frame)
(defhydra windows-hydra ()
  "Window Management"
  ("v" (flip-frame) "flip-vertically")
  ("h" (flop-frame) "flop-horizontally")
  ("r" (rotate-frame-clockwise) "rotate clockwise")
  ("<left>" (call-interactively #'shrink-window-horizontally)
   "shrink-window-horizontally")
  ("<right>" (call-interactively #'enlarge-window-horizontally)
   "enlarge-window-horizontally")
  ("<down>" (call-interactively #'shrink-window)
   "shrink-window")
  ("<up>" (call-interactively #'enlarge-window)
   "enlarge-window")
  ("q" nil "Quit"))
