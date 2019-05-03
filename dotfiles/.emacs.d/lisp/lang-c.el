;;; -*- lexical-binding: t; -*-

;; from prelude

(defun prelude-c-mode-common-defaults ()
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(setq prelude-c-mode-common-hook 'prelude-c-mode-common-defaults)

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook (lambda ()
                                (run-hooks 'prelude-c-mode-common-hook)))

(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))

;; from radian

;; (defun radian--flycheck-c/c++-setup ()
;;   "Disable some Flycheck checkers for CC modes.
;; This function is for use in `c-mode-hook' and `c++-mode-hook'."
;;   ;; These checkers are usually not accurate enough to find proper
;;   ;; headers and such. Disable them by default.
;;   (radian--flycheck-disable-checkers 'c/c++-clang 'c/c++-gcc))
;; (add-hook 'c-mode-hook #'radian--flycheck-c/c++-setup)
;; (add-hook 'c++-mode-hook #'radian--flycheck-c/c++-setup)

;; Package `irony-mode' provides a framework to use libclang to get
;; semantic information about C, C++, and Objective-C code. Such
;; information can be used by Company, ElDoc, or other packages.
(use-package irony
  :init

  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode)

  :config

  ;; This tells `irony-mode' to discover compile options in a
  ;; .clang_complete file or another similar format automatically. See
  ;; [1] for further discussion.
  ;;
  ;; [1]: https://github.com/Sarcasm/irony-mode#configuration
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))

;; Package `company-irony' provides a Company backend that uses Irony
;; to complete symbols. See also `company-irony-c-headers'.
(use-package company-irony
  :demand t
  :after (:all company irony))

;; Package `company-irony-c-headers' provides a Company backend that
;; uses Irony to complete header file #includes. See also
;; `company-irony'.
(use-package company-irony-c-headers
  :demand t
  :after company-irony
  :config

  (radian-defhook radian--company-irony-setup ()
    irony-mode-hook
    "Configure Company to use Irony as a backend."
    (setq-local company-backends
                (cons (list #'company-irony #'company-irony-c-headers)
                      radian--company-backends-global))))

;; Package `irony-eldoc' provides an ElDoc backend that uses Irony to
;; display function signatures.
(use-package irony-eldoc
  :demand t
  :after irony
  :config

  (add-hook 'irony-mode-hook #'irony-eldoc))

;; Package `flycheck-irony' provides a Flycheck syntax checker that
;; uses Irony to display compilation errors and warnings.
(use-package flycheck-irony
  :demand t
  :after (:all flycheck irony)
  :config

  ;; This setup is global, so no need to run it on a mode hook.
  (flycheck-irony-setup)

  ;; Also use cppcheck. See [1] for discussion.
  ;;
  ;; [1]: https://github.com/Sarcasm/flycheck-irony/issues/9
  (flycheck-add-next-checker 'irony '(warning . c/c++-cppcheck)))

(provide 'lang-c)
