;;; -*- lexical-binding: t; -*-

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com")

;; (setq debug-on-error t)

;; init
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'boiler-plate) ; where the magic happens

;; core configuration
(require 'cfg-even-better-defaults) ; like `better-defaults', but even better
(require 'cfg-functions) ; misc. handy functions
(require 'cfg-general) ; misc. settings and enhancements
(require 'cfg-global-keybinds) ; `exwm', `helm', `hydra', `smartparens', `crux' etc.
(require 'cfg-theme) ; everything theme-related

;; from here to EOF lazy-load (:defer) everything

;; ide-like features
(require 'dev-programming)

;;  support for things like docker and various configuration file formats
;; (require 'dev-ops)

;; programming configs, mostly adapted from prelude. https://github.com/bbatsov
(require 'lang-shell)
;; (require 'lang-c)
(require 'lang-lisp-generic)
(require 'lang-elisp)
;; (require 'lang-common-lisp)
;; (require 'lang-clojure)
;; (require 'lang-scheme)
(require 'lang-nix)
;; (require 'lang-web)
;; (require 'lang-css)
;; (require 'lang-js)
;; (require 'lang-perl)
;; (require 'lang-python)
;; (require 'lang-ruby)

;; applications
(require 'apps-gnus) ; email client
(require 'apps-erc) ; irc client
(require 'apps-emms) ; mpv player front-end
(require 'apps-pdf-tools) ; more performant pdf viewer

;;; init.el ends here
