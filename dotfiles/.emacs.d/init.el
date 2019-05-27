;;; -*- lexical-binding: t; -*-

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'boiler-plate)
(require 'cfg-even-better-defaults)
(require 'cfg-helper-functions)
(require 'cfg-general-settings)
(require 'cfg-global-keybinds) ;; exwm,helm,hydra,crux,smartparens,etc.
(require 'cfg-theme)

(require 'lang-programming-tools) ;; yasnippet,company,dumbjump,projectile,magit,flycheck,etc.
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
