;;; -*- lexical-binding: t; -*-

(use-package rainbow-delimiters)

(defun prelude-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq prelude-lisp-coding-hook 'prelude-lisp-coding-defaults)

(defun prelude-interactive-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq prelude-interactive-lisp-coding-hook 'prelude-interactive-lisp-coding-defaults)

(provide 'lang-lisp-generic)
