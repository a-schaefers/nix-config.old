;;; -*- lexical-binding: t; -*-

(defun my-custom-startup ()
  (interactive)
  ;; open a persistent scratch buffer
  (find-file (concat "~/" (user-login-name) ".el")))

(defun my-find-user-init-file ()
  "Edit the `user-init-file'"
  (interactive)
  (find-file user-init-file))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current WINDOW."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defun spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
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

(provide 'cfg-helper-functions)
