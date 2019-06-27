;;; -*- lexical-binding: t; -*-

(require 'xelb)
(require 'exwm)
(setq exwm-workspace-number 8)

(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

(defun my-toggle-redshift ()
  (interactive)
  (start-process-shell-command "pkill" nil "pkill -USR1 '^redshift'"))

(defun my-tray-apps ()
  (start-process-shell-command "redshift-gtk" nil
                               "redshift-gtk -l 43.3665:-124.2179 -t 5500:2000 -b 1:1")
  (start-process-shell-command "nm-applet" nil "nm-applet")
  (start-process-shell-command "volumeicon" nil "volumeicon")
  (start-process-shell-command "udiskie" nil "udiskie -t"))

(add-hook 'exwm-init-hook 'my-tray-apps)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "VGA-1" 1 "VGA-1" 2 "VGA-1" 3 "VGA-1"
                                            4"LVDS-1" 5 "LVDS-1" 6 "LVDS-1" 7 "LVDS-1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output VGA-1 --left-of LVDS-1")))
(exwm-randr-enable)
(exwm-enable)

(setq exwm-input-global-keys
      `(
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "<s-f%d>" (1+ i))) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(setq exwm-input-simulation-keys
      '(
        ([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ([?\C-s] . [?\C-f])))

(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(exwm-input-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(exwm-input-set-key (kbd "s-1") 'delete-other-windows)
(exwm-input-set-key (kbd "s-2") 'split-window-below)
(exwm-input-set-key (kbd "s-3") 'split-window-right)
(exwm-input-set-key (kbd "s-0") 'delete-window)
(exwm-input-set-key (kbd "<s-tab>") 'ace-window)
(exwm-input-set-key (kbd "s--") 'kill-this-buffer)
(exwm-input-set-key (kbd "<s-backspace>") 'kill-buffer-and-window)
(exwm-input-set-key (kbd "<f9>") 'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "<f10>") 'my-toggle-redshift)

(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

(require 'desktop-environment)
(desktop-environment-mode)
(exwm-input-set-key
 (kbd "<s-kp-multiply>") 'desktop-environment-toggle-mute)
(exwm-input-set-key
 (kbd "<s-kp-add>") 'desktop-environment-volume-increment)
(exwm-input-set-key
 (kbd "<s-kp-subtract>") 'desktop-environment-volume-decrement)

(require 'edit-server)
(edit-server-start)
