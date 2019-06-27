;;; -*- lexical-binding: t; -*-

(require 'better-defaults)

(defun spacemacs/alternate-buffer (&optional window)
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defun spacemacs/alternate-window ()
  (interactive)
  (let ((prev-window (get-mru-window nil t t)))
    (unless prev-window (user-error "Last window not found"))
    (select-window prev-window)))

(global-set-key (kbd "M-g") 'keyboard-quit)
(global-set-key (kbd "C-x <tab>") 'spacemacs/alternate-buffer)
(global-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<C-tab>") 'spacemacs/alternate-buffer))
(global-set-key (kbd "<s-backspace>") 'kill-buffer-and-window)
(global-set-key (kbd "s--") 'kill-this-buffer)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "<s-tab>") 'ace-window)
(global-set-key (kbd "<C-kp-add>") 'text-scale-increase)
(global-set-key (kbd "<C-kp-subtract>") 'text-scale-decrease)

(delete-selection-mode 1)
(blink-cursor-mode -1)
(setq visible-bell nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default fill-column 80)

(global-unset-key (kbd "C-z"))

(setq vc-follow-symlinks t)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(setq tab-always-indent 'complete)

(setq tramp-default-method "ssh")
(setq tramp-copy-size-limit nil)

(require 'recentf)
(setq recentf-max-saved-items 500
      recentf-max-menu-items 15)
(recentf-mode +1)

(require 'midnight)
(setq midnight-period 7200)
(midnight-mode 1)

(require 'winner)
(winner-mode 1)
