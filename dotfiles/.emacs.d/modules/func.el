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
