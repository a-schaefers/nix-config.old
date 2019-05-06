;;; -*- lexical-binding: t; -*-

;; Startup to text-mode, not prog-mode and gain .2 seconds init time lol
(setq inhibit-startup-screen t
      initial-major-mode 'text-mode)
;; kill scratch and load custom org-mode startup page :)
(add-hook 'after-init-hook 'my-kill-scratch)
(add-hook 'after-init-hook 'my-custom-startup)


;; Faster startup time for Emacs and potentially better perf on modern pc's.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 20000000)

            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Feature `gnutls' provides support for SSL/TLS connections, using
;; the GnuTLS library.
(with-eval-after-load 'gnutls
  (setq gnutls-log-level 0)
  ;; Do not allow insecure TLS connections.
  (setq gnutls-verify-error t)

  ;; Bump the required security level for TLS to an acceptably modern
  ;; value.
  (setq gnutls-min-prime-bits 3072))

;; Make Emacs verify tls certificates (depends: gnutls-cli certifi)
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

;; some elisp hacking libs
(require 'cl-lib)
(require 'map)
(require 'subr-x)

;; bootstrap https://github.com/jwiegley/use-package and setup melpa

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; by default, don't care about lazy loading or init time.
(setq use-package-always-ensure t)
(setq use-package-always-demand t)
(setq use-package-always-defer nil)

;; for a faster and more reliable bootstrap
(use-package async :config (async-bytecomp-package-mode 1))

;; keep up to date
(use-package auto-package-update :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 30 ; monthly
        auto-package-update-prompt-before-update nil)
  (auto-package-update-maybe))

(provide 'boiler-plate)
