;;; -*- lexical-binding: t; -*-

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

(load-directory "/nix-config/dotfiles/.emacs.d/lisp.d")

(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(when (file-exists-p custom-file)
  (load custom-file))
