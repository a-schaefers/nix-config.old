;;; -*- lexical-binding: t; -*-

(use-package inf-ruby :defer t)
(use-package yari :defer t)

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(define-key 'help-command (kbd "R") 'yari)

(with-eval-after-load 'ruby-mode
  (defun prelude-ruby-mode-defaults ()
    (inf-ruby-minor-mode +1)
    ;; CamelCase aware editing operations
    (subword-mode +1))

  (setq prelude-ruby-mode-hook 'prelude-ruby-mode-defaults)

  (add-hook 'ruby-mode-hook (lambda ()
                              (run-hooks 'prelude-ruby-mode-hook))))

(provide 'lang-ruby)
