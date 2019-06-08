;;; -*- lexical-binding: t; -*-

(use-package geiser :disabled :defer t
  :config
  (setq geiser-mode-start-repl-p t)

  (add-hook 'scheme-mode-hook (lambda ()
                                (run-hooks 'prelude-lisp-coding-hook))))

(use-package racket-mode :defer t
  :mode "\\.rkt\\'"
  :config
  (add-hook 'racket-mode-hook
            (lambda ()
              (define-key racket-mode-map (kbd "C-c C-b") 'racket-run)
              (define-key racket-mode-map (kbd "C-c C-c") 'racket-eval-last-sexp)))

  (add-hook 'racket-mode-hook (lambda ()
                                (run-hooks 'prelude-lisp-coding-hook)
                                (flycheck-mode -1))))

(provide 'lang-scheme)
