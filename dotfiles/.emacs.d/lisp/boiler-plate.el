;;; -*- lexical-binding: t; -*-

(setq inhibit-startup-screen t
      initial-major-mode 'text-mode)
(add-hook 'after-init-hook 'my-kill-scratch)
(add-hook 'after-init-hook 'my-custom-startup)

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 20000000)

            (require 'server)
            (unless (server-running-p)
              (server-start))))

(with-eval-after-load 'gnutls
  (setq gnutls-log-level 0)
  (setq gnutls-verify-error t)
  (setq gnutls-min-prime-bits 3072))

(start-process-shell-command  "check-for-gnutls" nil "which gnutls-cli || notify-send \"MISSING gnutls-cli\"")
(start-process-shell-command "check-for-certifi" nil "python -m certifi | grep .pem || notify-send \"MISSING certifi\"")
(setq tls-checktrust t)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile))))

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-always-demand t)
(setq use-package-always-defer nil)

(use-package async :config (async-bytecomp-package-mode 1))

(use-package auto-package-update :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 30
        auto-package-update-prompt-before-update nil)
  (auto-package-update-maybe))

(provide 'boiler-plate)
