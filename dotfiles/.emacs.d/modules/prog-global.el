;;; -*- lexical-binding: t; -*-

;; MISC

(defun my-prog-mode-hook ()
  (goto-address-prog-mode 1)
  (whitespace-mode 1))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(global-set-key (kbd "<f5>") 'compile)

(require 'browse-kill-ring)
(global-set-key (kbd "M-y") 'browse-kill-ring)

(require 'webpaste)
(global-set-key (kbd "C-c <print>") 'webpaste-paste-region)

(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))

(require 'paren)
(show-paren-mode 1)

(require 'elec-pair)
(electric-pair-mode 1)

(require 'magit)
(global-set-key (kbd "C-c g") #'magit-status)
(setq magit-diff-refine-hunk t)

(setq magit-repository-directories '(("~/repos" . 1)))
(when (file-directory-p "/nix-config")
  (add-to-list 'magit-repository-directories '("/nix-config" . 0)))

(with-eval-after-load 'projectile
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    (projectile-save-known-projects)))

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode 1)

(require 'dumb-jump)
(global-set-key (kbd "C-c .") #'dumb-jump-go)
(global-set-key (kbd "C-c ,") #'dumb-jump-back)
(dumb-jump-mode 1)

(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq flycheck-display-errors-function nil)
(global-flycheck-mode 1)

(require 'rainbow-delimiters)

(require 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

(require 'crux)
(crux-reopen-as-root-mode 1)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
(global-set-key (kbd "<C-S-return>") #'crux-smart-open-line-above)
(global-set-key (kbd "<S-return>") #'crux-smart-open-line)
(global-set-key (kbd "C-c r") #'crux-recentf-find-file)
(global-set-key (kbd "C-c R") #'crux-rename-buffer-and-file)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c D") #'crux-delete-buffer-and-file)
(global-set-key (kbd "C-c I") 'crux-find-user-init-file)

(require 'ace-window)
(global-set-key [remap other-window] 'ace-window)
(setq aw-scope 'frame)
(ace-window-display-mode -1)

(require 'aggressive-indent)
(aggressive-indent-global-mode 1)
