;;; -*- lexical-binding: t; -*-

(defun my-shell ()
  (interactive)
  (if (get-buffer "*shell*")
      (kill-buffer "*shell*"))
  (shell)
  (delete-other-windows))

(defun my-kill-scratch ()
  (kill-buffer "*scratch*"))

(defun my-kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun spacemacs/alternate-buffer (&optional window)
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defun spacemacs/alternate-window ()
  (interactive)
  (let ((prev-window (get-mru-window nil t t)))
    (unless prev-window (user-error "Last window not found"))
    (select-window prev-window)))

(defun my-toggle-redshift ()
  (interactive)
  (start-process-shell-command "pkill" nil "pkill -USR1 '^redshift'"))

(defvar saved-window-configuration nil)
(defun push-window-configuration ()
  (interactive)
  (push (current-window-configuration) saved-window-configuration))
(defun pop-window-configuration ()
  (interactive)
  (let ((config (pop saved-window-configuration)))
    (if config
        (set-window-configuration config)
      (if (> (length (window-list)) 1)
          (delete-window)
        (bury-buffer)))))

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

(provide 'my-functions)
