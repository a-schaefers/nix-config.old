;;;; BASH

(defun my-shell ()
  (interactive)
  (if (get-buffer "*shell*")
      (kill-buffer "*shell*"))
  (shell))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(setq explicit-shell-file-name "bash")
(require 'bash-completion)
(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
(defun my-shell-mode-hook()
  (setq-local compile-command
              '((lambda()
                  (save-buffer)
                  (async-shell-command (buffer-file-name))))))
(add-hook 'sh-mode-hook 'my-shell-mode-hook)

(require 'xterm-color)
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))
(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))


;;;; EMACS LISP & CLOJURE

(require 'lisp-mode)
(defun my-ielm ()
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'my-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

(require 'ielm)
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)

(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(require 'lispy)
(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'ielm-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'eval-expression-minibuffer-setup-hook (lambda () (lispy-mode 1)))
(setq lispy-compat '(edebug
                     cider))

(require 'clojure-mode)
(add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(require 'cider)
(setq nrepl-log-messages t)
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;;;; NIX

(require 'nix-mode)
