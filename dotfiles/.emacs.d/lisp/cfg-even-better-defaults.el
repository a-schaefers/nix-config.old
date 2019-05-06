;;; -*- lexical-binding: t; -*-

(use-package better-defaults :config

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

  (setq vc-follow-symlinks t)

  (setq find-file-visit-truename t)

  (setq find-file-suppress-same-file-warnings t)

  (delete-selection-mode 1)

  (defalias 'yes-or-no-p 'y-or-n-p)

  (prefer-coding-system 'utf-8)

  (global-set-key (kbd "<C-kp-add>") 'text-scale-increase)
  (global-set-key (kbd "<C-kp-subtract>") 'text-scale-decrease)

  (require 'winner)
  (winner-mode 1)

  (setq disabled-command-function nil)

  (setq ad-redefinition-action 'accept)

  (setq tramp-default-method "ssh")
  (setq tramp-copy-size-limit nil)

  (defun disable-all-themes ()
    (interactive)
    (dolist (i custom-enabled-themes)
      (disable-theme i)))
  (defadvice load-theme (before disable-themes-first activate)
    (disable-all-themes))

  (add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (require 'dired-loaddefs)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-auto-revert-buffer t)
  (setq dired-omit-verbose nil)
  (global-unset-key (kbd "C-z"))
  (setq ring-bell-function #'ignore)
  (setq echo-keystrokes 1e-6)
  (setq blink-matching-paren nil)
  (setq isearch-lazy-highlight-initial-delay 0)

  (unbind-key "<S-down-mouse-1>")
  (unbind-key "<S-down-mouse-3>")
  (unbind-key "<C-down-mouse-1>")
  (unbind-key "<C-down-mouse-2>")
  (unbind-key "<C-down-mouse-3>"))

(provide 'cfg-even-better-defaults)
