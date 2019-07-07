;;; -*- lexical-binding: t; -*-

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      inhibit-startup-screen t
      initial-major-mode 'emacs-lisp-mode
      gc-cons-threshold 100000000
      debug-on-error nil)

(toggle-frame-fullscreen)

(defun my-home ()
  (interactive)
  (when (get-buffer "*scratch*")
    (kill-buffer "*scratch*"))
  (if (get-buffer "*shell*")
      (switch-to-buffer "*shell*")
    (shell))
  (delete-other-windows)
  (cd "~/"))

(add-hook 'after-init-hook 'my-home)

(defun load-directory (directory)
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

(load-directory "/nix-config/external/.emacs.d/lisp.d")

(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'server)
(unless (server-running-p)
  (server-start))

(with-eval-after-load 'gnutls
  (setq gnutls-log-level 0)
  (setq gnutls-verify-error t)
  (setq gnutls-min-prime-bits 3072))
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
