;;; -*- lexical-binding: t; -*-

(use-package common-lisp-snippets :after yasnippet)
(use-package slime-company :after slime)
(use-package slime :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  :config
  (setq slime-lisp-implementations
        '((ccl ("ccl"))
          (clisp ("clisp" "-q"))
          (cmucl ("cmucl" "-quiet"))
          (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

  (if (and (eq system-type 'darwin)
           (executable-find "ccl"))
      (setq slime-default-lisp 'ccl)
    (setq slime-default-lisp 'sbcl))

  (setq slime-contribs '(slime-fancy slime-cl-indent slime-company))

  (add-hook 'lisp-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))
  (add-hook 'slime-repl-mode-hook (lambda ()
                                    (smartparens-strict-mode +1)
                                    (whitespace-mode -1)))

  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t
        slime-auto-start 'always)
  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))

(provide 'lang-common-lisp)
