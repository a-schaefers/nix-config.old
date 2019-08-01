;;; -*- lexical-binding: t; -*-

(defun my-toggle-redshift ()
  (interactive)
  (start-process-shell-command "pkill" nil "pkill -USR1 '^redshift'"))

(require 'xelb)
(require 'exwm)
(setq exwm-workspace-number 1)
(require 'exwm-config)
(exwm-config-default)
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

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
