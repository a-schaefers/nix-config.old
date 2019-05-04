;;; -*- lexical-binding: t; -*-

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'boiler-plate)
(require 'cfg-even-better-defaults)
(require 'cfg-functions)
(require 'cfg-general)
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

;;; init.el ends here
