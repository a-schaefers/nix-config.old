;;; -*- lexical-binding: t; -

;; make a script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; shell to use with shell-mode
(setq explicit-shell-file-name "bash")

;; ansi-term wrapper-- don't ask me, just use bash shell
(defun my-ansi-term ()
  (interactive)
  (ansi-term "bash"))

;; use real bash-comp with shell-mode
(use-package bash-completion :defer t
  :init
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))

(defun my-shell-mode-hook()
  "hook for shell scripts"

  ;; M-x compile will save and run the script, and post output to another window
  (setq-local compile-command '((lambda()
                                  (save-buffer)
                                  (async-shell-command (buffer-file-name))))))

(add-hook 'sh-mode-hook 'my-shell-mode-hook)

;; add 256 color support for shell and eshell
(use-package xterm-color
  ;;  Don't forget to setenv TERM xterm-256color
  :config(setq comint-output-filter-functions
               (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  ;; Also set TERM accordingly (xterm-256color)
  ;; You can also use it with eshell (and thus get color output from system ls):
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
