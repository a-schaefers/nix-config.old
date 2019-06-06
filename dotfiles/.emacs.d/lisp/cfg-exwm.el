;;; -*- lexical-binding: t; -*-

(if (string= (getenv "XDG_CURRENT_DESKTOP") "EXWM")
    (progn
      (require 'xelb)
      (require 'exwm)
      (setq exwm-workspace-number 8)

      ;; systray
      (require 'exwm-systemtray)
      (exwm-systemtray-enable)
      (setq exwm-systemtray-height 16)

      (defun my-startup ()
        ;; minimal apps
        (start-process-shell-command "redshift-gtk" nil
                                     "redshift-gtk -l 43.3665:-124.2179 -t 5500:2000 -b 1:1")
        (start-process-shell-command "network-manager-applet" nil "nm-applet")
        (start-process-shell-command "volumeicon" nil "volumeicon"))

      (add-hook 'exwm-init-hook 'my-startup)

      ;; randr
      (require 'exwm-randr)
      (setq exwm-randr-workspace-output-plist '(0 "VGA-1" 1 "VGA-1" 2 "VGA-1" 3 "VGA-1"
                                                  4"LVDS-1" 5 "LVDS-1" 6 "LVDS-1" 7 "LVDS-1"))
      (add-hook 'exwm-randr-screen-change-hook
                (lambda ()
                  (start-process-shell-command
                   "xrandr" nil "xrandr --output VGA-1 --left-of LVDS-1")))
      (exwm-randr-enable)
      (exwm-enable)))

(provide 'cfg-exwm)
