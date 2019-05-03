;;; -*- lexical-binding: t; -*-

(use-package clojure-mode :defer t)
(use-package cider :defer t)
(use-package clojure-snippets :after yasnippet)

(with-eval-after-load 'clojure-mode
  (defun prelude-clojure-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'prelude-lisp-coding-hook))

  (setq prelude-clojure-mode-hook 'prelude-clojure-mode-defaults)

  (add-hook 'clojure-mode-hook (lambda ()
                                 (run-hooks 'prelude-clojure-mode-hook))))

(with-eval-after-load 'cider
  (setq nrepl-log-messages t)

  (add-hook 'cider-mode-hook 'eldoc-mode)

  (defun prelude-cider-repl-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'prelude-interactive-lisp-coding-hook))

  (setq prelude-cider-repl-mode-hook 'prelude-cider-repl-mode-defaults)

  (add-hook 'cider-repl-mode-hook (lambda ()
                                    (run-hooks 'prelude-cider-repl-mode-hook))))

(provide 'lang-clojure)
