;;; -*- lexical-binding: t; -*-

(use-package yasnippet-snippets :defer t)
(use-package yasnippet :defer t
  :init (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package company :defer t
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-global-modes '(not shell-mode))
  (setq company-idle-delay 0)

  (setq company-transformers '(company-sort-by-occurrence))

  (setq company-show-numbers t)

  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all company backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (setq company-frontends '(company-pseudo-tooltip-frontend))

  (setq company-tooltip-minimum company-tooltip-limit)

  (setq company-require-match nil))

(use-package dumb-jump :defer t
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look))
  :init (dumb-jump-mode 1)
  (setq dumb-jump-selector 'helm))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package aggressive-indent :defer t
  :init (add-hook 'prog-mode-hook 'aggressive-indent-mode))

(setq whitespace-style '(face empty tabs lines-tail trailing))
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'prog-mode-hook 'whitespace-mode)

(use-package projectile :defer t
  :bind (("C-c p" . projectile-command-map))
  :config (projectile-mode t)
  (setq projectile-completion-system 'helm))

(use-package magit :defer t
  :bind (("C-c gs" . magit-status)
         ("C-c gl" . magit-log)
         ("C-c gf" . magit-log-buffer-file)
         ("C-c gb" . magit-blame))
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-repository-directories '(("~/repos" . 1)))

  (when (file-directory-p "/nix-config")
    (add-to-list 'magit-repository-directories '("/nix-config" . 0)))

  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    (projectile-save-known-projects)))

(use-package smerge-mode)

(use-package flycheck
  :bind (("C-c f" . flycheck-mode))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (setq flycheck-display-errors-function nil))

(use-package rg :defer t)

(use-package ag :defer t)

(use-package webpaste :defer t)

(provide 'lang-programming-tools)
