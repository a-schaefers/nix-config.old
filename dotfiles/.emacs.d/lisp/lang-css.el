;;; -*- lexical-binding: t; -*-

;; css

(use-package rainbow-mode :defer t)
(with-eval-after-load 'css-mode
  (setq css-indent-offset 2)

  (defun prelude-css-mode-defaults ()
    (rainbow-mode +1)
    (run-hooks 'prelude-prog-mode-hook))

  (setq prelude-css-mode-hook 'prelude-css-mode-defaults)

  (add-hook 'css-mode-hook (lambda ()
                             (run-hooks 'prelude-css-mode-hook))))


;; scss

(use-package scss-mode :defer t)

;; turn off annoying auto-compile on save
(setq scss-compile-at-save nil)

(defun prelude-scss-mode-defaults ()
  (prelude-css-mode-defaults))

(setq prelude-scss-mode-hook 'prelude-scss-mode-defaults)

(add-hook 'scss-mode-hook (lambda ()
                            (run-hooks 'prelude-scss-mode-hook)))

(provide 'lang-css)
