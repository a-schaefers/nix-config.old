;;; -*- lexical-binding: t; -*-

(use-package elisp-slime-nav)
(use-package rainbow-mode)

(defun prelude-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'prelude-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun prelude-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun prelude-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'prelude-lisp-coding-hook)
  (eldoc-mode +1)
  (rainbow-mode +1)
  (setq mode-name "EL")
  (prelude-conditional-emacs-lisp-checker))

(setq prelude-emacs-lisp-mode-hook 'prelude-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'prelude-emacs-lisp-mode-hook)))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(defun prelude-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'prelude-interactive-lisp-coding-hook)
  (eldoc-mode +1))

(setq prelude-ielm-mode-hook 'prelude-ielm-mode-defaults)

(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'prelude-ielm-mode-hook)))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(with-eval-after-load 'checkdoc
  (put 'checkdoc-package-keywords-flag 'safe-local-variable #'booleanp))

(with-eval-after-load 'elisp-lint
  (put 'elisp-lint-indent-specs 'safe-local-variable #'listp))

(provide 'lang-elisp)
