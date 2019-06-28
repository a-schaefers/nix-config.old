;;; -*- lexical-binding: t; -*-

;; put hydra menus on <Caps Lock> via setxkbmap -option caps:menu and then bind hydra to <menu>

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
  ("a" (my-insert-contact) "abook")
  ("e" (gnus) "email")
  ("i" (my-erc) "irc")
  ("b" (call-interactively #'eww) "eww")
  ("w" (windows-hydra/body) "win")
  ("<menu>" nil))

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
