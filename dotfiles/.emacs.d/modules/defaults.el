;;; -*- lexical-binding: t; -*-

(require 'better-defaults)

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
