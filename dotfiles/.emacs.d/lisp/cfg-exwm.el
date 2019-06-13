;;; -*- lexical-binding: t; -*-

(if (string= (getenv "XDG_CURRENT_DESKTOP") "EXWM")
    (progn
      (require 'xelb)
      (require 'exwm)
      (setq exwm-workspace-number 8)

      (require 'exwm-systemtray)
      (exwm-systemtray-enable)
      (setq exwm-systemtray-height 16)

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
      (exwm-enable)))

(use-package desktop-environment :after exwm
  :config
  (desktop-environment-mode)
  (exwm-input-set-key
   (kbd "<s-kp-multiply>") 'desktop-environment-toggle-mute)
  (exwm-input-set-key
   (kbd "<s-kp-add>") 'desktop-environment-volume-increment)
  (exwm-input-set-key
   (kbd "<s-kp-subtract>") 'desktop-environment-volume-decrement))

(provide 'cfg-exwm)
