;;; -*- lexical-binding: t; -*-

(defun my-custom-startup ()
  (interactive)
  (find-file (concat "~/" (user-login-name) ".el")))

(defun my-find-user-init-file ()
  "Edit the `user-init-file'"
  (interactive)
  (find-file user-init-file))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current WINDOW."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defun spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the current frame."
  (interactive)
  (let ((prev-window (get-mru-window nil t t)))
    (unless prev-window (user-error "Last window not found"))
    (select-window prev-window)))

(defun my-kill-scratch ()
  (kill-buffer "*scratch*"))

(defun my-kill-all-buffers ()
  "Kill all buffers without mercy."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun whitespace-cleanup-enable ()
  (interactive)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (message "whitespace cleanup enabled"))

(defun whitespace-cleanup-disable ()
  (interactive)
  (remove-hook 'before-save-hook 'whitespace-cleanup)
  (message "whitespace cleanup disabled"))

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

(provide 'cfg-helper-functions)
