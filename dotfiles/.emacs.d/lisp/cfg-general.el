;;; -*- lexical-binding: t; -*-

;; default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chrome")

;; clickable code comments e.g. http://www.google.com
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.0)
  (which-key-mode))

;; make dired transactions asynchronous
(use-package async :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

;; cleanup buffer list every two hours
(use-package midnight
  :config
  (setq midnight-period 7200)
  (midnight-mode 1))

;; org-mode setup
(setq org-agenda-files
      (append
       (file-expand-wildcards "~/org/*.org")))
(setq org-confirm-babel-evaluate nil)
(defun my-org-hook ()
  (company-mode 1)
  (smartparens-mode 1))
(add-hook 'org-mode-hook 'my-org-hook)

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Disable warnings from obsolete advice system.
(setq ad-redefinition-action 'accept)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60)
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq  recentf-max-saved-items 500
       recentf-max-menu-items 15)
(recentf-mode +1)

(require 'eldoc)
;; Always truncate ElDoc messages to one line. This prevents the
;; echo area from resizing itself unexpectedly when point is on a
;; variable with a multiline docstring.
(setq eldoc-echo-area-use-multiline-p nil)

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp :defer t
  :bind (("M-%" . vr/query-replace)))

(provide 'cfg-general)
