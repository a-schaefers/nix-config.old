;;; -*- lexical-binding: t; -*-

(setq init-file-debug nil)
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(setq debug-on-error t)
(setq debug-on-quit t)
(setq edebug-all-forms t)

(with-eval-after-load 'exwm
  (exwm-debug 1))

(setq user-full-name "Adam Schaefers"
user-mail-address "paxchristi888@gmail.com")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'boiler-plate)

(require 'cfg-even-better-defaults)
(require 'cfg-helper-functions)
(require 'cfg-general-settings)
(require 'cfg-exwm)
(require 'cfg-global-keybinds)
(require 'cfg-theme)

(require 'lang-programming-tools)
(require 'lang-nix)
(require 'lang-shell)
(require 'lang-lisp-generic)
(require 'lang-elisp)
(require 'lang-common-lisp)
(require 'lang-clojure)
(require 'lang-scheme)
(require 'lang-web)
(require 'lang-css)
(require 'lang-js)

(require 'apps-gnus)
(require 'apps-erc)
(require 'apps-pdf-tools)
(require 'apps-emms)

;;; init.el ends here
