;;; -*- lexical-binding: t; -*-

(require 'xelb)
(require 'exwm)
(setq exwm-workspace-number 1)

(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

(defun my-toggle-redshift ()
  (interactive)
  (start-process-shell-command "pkill" nil "pkill -USR1 '^redshift'"))

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

(exwm-input-set-key (kbd "<s-tab>") 'ace-window)
(exwm-input-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(exwm-input-set-key (kbd "<s-backspace>") 'kill-buffer-and-window)
(exwm-input-set-key (kbd "s--") 'kill-this-buffer)
(exwm-input-set-key (kbd "s-1") 'delete-other-windows)
(exwm-input-set-key (kbd "s-2") 'split-window-below)
(exwm-input-set-key (kbd "s-3") 'split-window-right)
(exwm-input-set-key (kbd "s-0") 'delete-window)

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
