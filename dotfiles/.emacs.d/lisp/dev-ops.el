;;; -*- lexical-binding: t; -*-

;; NOTE: currently installing ebuild-mode via emerge...

(use-package keychain-environment
  :load-path (lambda () (expand-file-name "lisp-extra" user-emacs-directory))
  :commands keychain-refresh-environment
  :ensure nil)

(use-package terraform-mode :defer t)

(use-package dockerfile-mode :defer t)
(use-package docker-compose-mode :defer t)
(use-package docker :defer t)
(use-package docker-tramp)

(use-package crontab-mode :load-path (lambda () (expand-file-name "lisp-extra" user-emacs-directory))
  :ensure nil :defer t
  :commands crontab-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode)))
(use-package systemd :defer t)

(use-package apache-mode :defer t)

(use-package nginx-mode :defer t)

(use-package gitconfig-mode :defer t)

(use-package gitignore-mode :defer t)

(use-package json-mode :defer t)

(use-package pip-requirements :defer t)

(use-package pkgbuild-mode :defer t
  :init
  (add-to-list 'auto-mode-alist '("/PKGBUILD\\'" . pkgbuild-mode))
  (add-to-list 'auto-mode-alist '("/PKGBUILD-git\\'" . pkgbuild-mode)))

(use-package ssh-config-mode :defer t)

(use-package toml-mode :defer t)

(use-package yaml-mode :defer t
  :init
  ;; yaml-mode doesn't derive from prog-mode, but we can at least enable
  ;; whitespace-mode and apply cleanup.
  (add-hook 'yaml-mode-hook 'whitespace-mode)
  (add-hook 'yaml-mode-hook 'subword-mode))

(require 'nxml-mode)
(push '("<\\?xml" . nxml-mode) magic-mode-alist)
(add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode)) ; pom files should be treated as xml files
(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)
(setq nxml-auto-insert-xml-declaration-flag nil)
(setq nxml-bind-meta-tab-to-complete-flag t)
(setq nxml-slash-auto-complete-flag t)

(provide 'dev-ops)
