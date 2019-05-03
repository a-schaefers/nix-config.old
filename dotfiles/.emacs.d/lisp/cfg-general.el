;;; -*- lexical-binding: t; -*-

;; skip gnome-keyring and use loopback
;; (setq epa-pinentry-mode 'loopback)

;; default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chrome")

;; clickable code comments e.g. http://www.google.com
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set helm + which-key to the same, 25% window height
(use-package shackle :disabled
  :config
  (setq helm-display-function 'pop-to-buffer)
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.30)))
  (shackle-mode 1))
(use-package which-key
  :config
  ;; (setq which-key-side-window-max-height 0.30)
  (setq which-key-idle-delay 0.0)
  (which-key-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; nag in prog-mode if not clocked in
;; TODO use a projectile mode hook instead
(use-package org-nag-clock :disabled
  :load-path (lambda () (expand-file-name "lisp-extra" user-emacs-directory))
  :config
  ;; enable / disable
  (org-nag-clock-mode 1))

(setq org-agenda-files
      (append
       (file-expand-wildcards "~/org/*.org")))
(setq org-confirm-babel-evaluate nil)

(defun my-org-hook ()
  (company-mode 1)
  (smartparens-mode 1))
(add-hook 'org-mode-hook 'my-org-hook)

;; RADIAN [code below] is a really nice Emacs project from the author of
;; straight-use-package and el-patch
;; https://github.com/raxod502/radian

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Disable warnings from obsolete advice system. They don't provide
;; useful diagnostic information and often they can't be fixed except
;; by changing packages upstream.
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

;; Package `visual-regexp-steroids' allows `visual-regexp' to use
;; regexp engines other than Emacs'; for example, Python or Perl
;; regexps.
(use-package visual-regexp-steroids :defer t
  :bind (("C-r" . vr/isearch-backward)
         ("C-s" . vr/isearch-forward))
  :init
  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs))

;; Original code from
;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
;; thanks!
(radian-defadvice radian--advice-disable-eldoc-on-flycheck
    (&rest _)
  :after-while eldoc-display-message-no-interference-p
  "Disable ElDoc when point is on a Flycheck overlay.
This prevents ElDoc and Flycheck from fighting over the echo
area."
  (not (and (bound-and-true-p flycheck-mode)
            (flycheck-overlay-errors-at (point)))))

(provide 'cfg-general)
