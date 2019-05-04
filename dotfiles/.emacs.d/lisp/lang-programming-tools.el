;;; -*- lexical-binding: t; -*-

;; snippets and auto-completion
(use-package yasnippet-snippets :defer t)
(use-package yasnippet :defer t
  :init (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package company :defer t
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-global-modes '(not shell-mode))
  (setq company-idle-delay 0)

  ;; weight by frequency
  (setq company-transformers '(company-sort-by-occurrence))

  ;; M+[0-9] selects by number
  (setq company-show-numbers t)

  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)

  ;; yas integration
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all company backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; allow the typing non-matching input
  (setq company-require-match nil))

;; jump to def without need of external configuration / tag generation.
(use-package dumb-jump :defer t
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look))
  :init (dumb-jump-mode 1)
  (setq dumb-jump-selector 'helm))

;; add per project and per language code formatting abilities
(use-package editorconfig
  :config (editorconfig-mode 1))

;; always indent automatically
(use-package aggressive-indent :defer t
  :init (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;; highlight whitespace and > 80 columns, auto strip whitespace.
(setq whitespace-style '(face empty tabs lines-tail trailing))
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; add project awareness
(use-package projectile :defer t
  :bind (("C-c p" . projectile-command-map))
  :config (projectile-mode t)
  (setq projectile-completion-system 'helm))

;; a powerful frontend to git
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

  ;; add magit's list of known repos automatically to projectile project list
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)))

;; Feature `smerge-mode' provides an interactive mode for visualizing
;; and resolving Git merge conflicts.
(use-package smerge-mode)

;; enabling flycheck globally is unsafe so enable per-buffer when needed instead.
;; TODO consider a projectile mode hook?
(use-package flycheck
  :bind (("C-c f" . flycheck-mode))
  :config
  ;; Add support for flycheck to follow `require' etc.
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Disable any checkers that are annoying here, such as `emacs-lisp-checkdoc'
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  ;; disable flycheck error popups
  (setq flycheck-display-errors-function nil))

;; Package `rg' just provides an interactive command `rg' to run the
;; search tool of the same name.
(use-package rg :defer t)

;; the same for `ag'
(use-package ag :defer t)

;; Package `webpaste' provides Emacs support for many different
;; command-line pastebins.
(use-package webpaste :defer t)

(provide 'lang-programming-tools)
