(require 'hydra)

;; put hydras on <Caps Lock>
(with-eval-after-load 'exwm
  (exwm-input-set-key (kbd "<menu>") 'caps-hydra/body))

(defhydra caps-hydra (:exit t)
  "Menu"
  ("!" (lambda (command)
         (interactive (list (read-shell-command "$ ")))
         (start-process-shell-command command nil command)) "cmd")
  ("s" (my-shell) "sh")
  ("e" (gnus) "email")
  ("i" (my-irc) "irc")
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
