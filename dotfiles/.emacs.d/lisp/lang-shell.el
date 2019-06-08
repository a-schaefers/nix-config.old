;;; -*- lexical-binding: t; -

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq explicit-shell-file-name "bash")

(defun my-ansi-term ()
  (interactive)
  (ansi-term "bash"))

(use-package bash-completion :defer t
  :init
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))

(defun my-shell-mode-hook()
  "hook for shell scripts"

  (setq-local compile-command '((lambda()
                                  (save-buffer)
                                  (async-shell-command (buffer-file-name))))))

(add-hook 'sh-mode-hook 'my-shell-mode-hook)

(use-package xterm-color
  :config(setq comint-output-filter-functions
               (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (require 'eshell)
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(defun my-shell-refresh ()
  "this could probably be better"
  (interactive)
  (setq kill-buffer-query-functions nil)
  (if (get-buffer "*shell*")
      (kill-buffer "*shell*"))
  (shell)
  (delete-other-windows)
  (setq kill-buffer-query-functions
        '(process-kill-buffer-query-function)))

(provide 'lang-shell)
