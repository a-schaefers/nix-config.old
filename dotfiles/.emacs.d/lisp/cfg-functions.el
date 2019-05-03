;;; -*- lexical-binding: t; -*-

(defun my-custom-startup ()
  (interactive)
  ;; open a persistent scratch buffer
  (find-file (concat "~/" (user-login-name) ".el"))
  (cd "~/"))

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

(defun radian--flycheck-disable-checkers (&rest checkers)
  "Disable the given Flycheck syntax CHECKERS, symbols.
This function affects only the current buffer, and neither causes
nor requires Flycheck to be loaded."
  (unless (boundp 'flycheck-disabled-checkers)
    (setq flycheck-disabled-checkers nil))
  (make-local-variable 'flycheck-disabled-checkers)
  (dolist (checker checkers)
    (cl-pushnew checker flycheck-disabled-checkers)))

(defun radian-reload-init ()
  (interactive)
  (message "Reloading init-file...")
  (load user-init-file nil 'nomessage)
  (message "Reloading init-file...done"))

(defun my-compton-toggle ()
  (interactive)
  (start-process-shell-command "compton" nil "pkill compton || compton --backend glx"))

(defun my-redshift-toggle ()
  (interactive)
  (start-process-shell-command "redshift" nil "pkill -USR1 redshift || redshift -l 43.3665:-124.2179"))

(provide 'cfg-functions)
