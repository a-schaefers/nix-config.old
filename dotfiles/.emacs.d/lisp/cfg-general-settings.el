;;; -*- lexical-binding: t; -*-

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(use-package edit-server
  :config
  (setq edit-server-new-frame nil)
  (edit-server-start))

(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.0)
  (which-key-mode))

(use-package async :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(use-package midnight
  :config
  (setq midnight-period 7200)
  (midnight-mode 1))

(setq org-agenda-files
      (append
       (file-expand-wildcards "~/org/*.org")))
(setq org-confirm-babel-evaluate nil)
(defun my-org-hook ()
  (company-mode 1)
  (smartparens-mode 1))
(add-hook 'org-mode-hook 'my-org-hook)

(require 'savehist)
(setq savehist-additional-variables
      '(search-ring regexp-search-ring)
      savehist-autosave-interval 60)
(savehist-mode +1)

(require 'recentf)
(setq  recentf-max-saved-items 500
       recentf-max-menu-items 15)
(recentf-mode +1)

(require 'eldoc)
(setq eldoc-echo-area-use-multiline-p nil)

(use-package visual-regexp :defer t
  :bind (("M-%" . vr/query-replace)))

(provide 'cfg-general-settings)
