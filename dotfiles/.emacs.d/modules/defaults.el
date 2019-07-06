;;; -*- lexical-binding: t; -*-

;; swapping left-ctl with left-alt is the best way to "ergo" Emacs.
(start-process-shell-command "setxkbmap" nil
                             "setxkbmap -option ctrl:swap_lalt_lctl")

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      inhibit-startup-screen nil
      initial-major-mode 'emacs-lisp-mode
      gc-cons-threshold 100000000
      debug-on-error nil)

(require 'better-defaults)

(add-hook 'text-mode-hook 'goto-address-mode)

(global-set-key (kbd "<C-kp-add>") 'text-scale-increase)
(global-set-key (kbd "<C-kp-subtract>") 'text-scale-decrease)

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
    (unless prev-window (user-error "Last window not found"))nn
    (select-window prev-window)))

(defun my-home ()
  (interactive)
  (cd "~/")
  (about-emacs)
  (crux-kill-other-buffers))
(global-set-key (kbd "<home>") 'my-home)

(global-set-key (kbd "<s-tab>") 'ace-window)
(global-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(global-set-key (kbd "<s-backspace>") 'kill-buffer-and-window)
(global-set-key (kbd "s--") 'kill-this-buffer)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-0") 'delete-window)

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
(global-unset-key (kbd "C-x C-z"))

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

;; use eww as the default web browser
(require 'eww)
(setq browse-url-browser-function 'eww-browse-url)

(defun my-external-browser (url)
  (start-process-shell-command "brave" nil (concat "brave " url)))

(defun eww-open-with-mpv ()
  (interactive)
  (eww-copy-page-url)
  (start-process-shell-command "mpv" nil (concat "mpv " (nth 0 kill-ring))))

;; opened by eww with "&" key
(setq shr-external-browser 'my-external-browser)

;; browse youtube videos from eww/mpv/youtube-dl with "^" key
(define-key eww-mode-map (kbd "^") 'eww-open-with-mpv)
