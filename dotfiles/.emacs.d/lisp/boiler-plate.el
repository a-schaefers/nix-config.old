;;; -*- lexical-binding: t; -*-

;; Startup to text-mode, not prog-mode and gain .2 seconds init time lol
(setq inhibit-startup-screen t
      initial-major-mode 'text-mode)
;; kill scratch and load custom org-mode startup page :)
(add-hook 'after-init-hook 'my-kill-scratch)
(add-hook 'after-init-hook 'my-custom-startup)

;; Faster startup time for Emacs and potentially better perf on modern pc's.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 20000000)

            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Feature `gnutls' provides support for SSL/TLS connections, using
;; the GnuTLS library.
(with-eval-after-load 'gnutls
  (setq gnutls-log-level 0)
  ;; Do not allow insecure TLS connections.
  (setq gnutls-verify-error t)

  ;; Bump the required security level for TLS to an acceptably modern
  ;; value.
  (setq gnutls-min-prime-bits 3072))

;; Make Emacs verify tls certificates (depends: gnutls-cli certifi)
(setq tls-checktrust t)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile))))

;; some elisp hacking libs
(require 'cl-lib)
(require 'map)
(require 'subr-x)

;; bootstrap https://github.com/jwiegley/use-package and setup melpa

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; by default, don't care about lazy loading or init time.
(setq use-package-always-ensure t)
(setq use-package-always-demand t)
(setq use-package-always-defer nil)

;; for a faster and more reliable bootstrap
(use-package async :config (async-bytecomp-package-mode 1))

;; keep up to date
(use-package auto-package-update :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 30 ; monthly
        auto-package-update-prompt-before-update nil)
  ;; this sucks, but basically dont start using this auto-update feature until the third run of Emacs
  (if (and (file-exists-p (concat user-emacs-directory ".first_run"))
           (file-exists-p (concat user-emacs-directory ".second_run")))
      (auto-package-update-maybe)
    (if (not (file-exists-p (concat user-emacs-directory ".first_run")))
        (write-region "" nil (concat user-emacs-directory ".first_run"))
      (if (not (file-exists-p (concat user-emacs-directory ".second_run")))
          (write-region "" nil (concat user-emacs-directory ".second_run"))))))

;; RADIAN is a really nice Emacs project from the author of
;; straight-use-package and el-patch
;; https://github.com/raxod502/radian
;; The following boiler code makes it easier to integrate from radian

;; Package `el-patch' provides a way to override the definition of an
;; internal function from another package by providing an s-expression
;; based diff which can later be validated to ensure that the upstream
;; definition has not changed.
(use-package el-patch :defer t)

;;; Define Radian customization group

(defgroup radian nil
  "Customize your Radian Emacs experience."
  :prefix "radian-"
  :group 'emacs
  :link '(url-link :tag "GitHub" "https://github.com/raxod502/radian"))

;;; Define utility functions and variables

(defmacro radian-protect-macros (&rest body)
  "Eval BODY, protecting macros from incorrect expansion.
Any code in a `with-eval-after-load' form that uses macros
defined in the form being `with-eval-after-load'ed should be
wrapped in this macro; otherwise, its correct evaluation is not
guaranteed by Elisp."
  (declare (indent 0))
  `(eval '(progn ,@body)))

(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. DOCSTRING and BODY are as in
`defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "radian-defadvice: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (advice-add ',place ',where #',name)
     ',name))

(defmacro radian-defhook (name arglist hook docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOK is the hook to which to add the
function. DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (string-match-p "-hook$" (symbol-name hook))
    (error "Symbol `%S' is not a hook" hook))
  (unless (stringp docstring)
    (error "radian-defhook: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(format "%s\n\nThis function is for use in `%S'."
                docstring hook)
       ,@body)
     (add-hook ',hook ',name)))

(defmacro radian-operating-system-p (os)
  "Return non-nil if OS matches the system type.
Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (pcase os
    (`unix `(not (memq system-type '(ms-dos windows-nt cygwin))))
    ((or `macOS `osx) `(eq system-type 'darwin))
    (`linux `(not (memq system-type
                        '(darwin ms-dos windows-nt cygwin))))
    (`windows `(memq system-type '(ms-dos windows-nt cygwin)))))

(defmacro radian-with-operating-system (os &rest body)
  "If OS matches the system type, eval and return BODY. Else return nil.
Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (declare (indent 1))
  `(when (radian-operating-system-p ,os)
     ,@body))

(defmacro radian--with-silent-load (&rest body)
  "Execute BODY, silencing any calls to `load' within."
  (declare (indent 0))
  `(cl-letf* ((load-orig (symbol-function #'load))
              ((symbol-function #'load)
               (lambda (file &optional noerror _nomessage &rest args)
                 (apply load-orig file noerror 'nomessage args))))
     ,@body))

(defmacro radian--with-silent-write (&rest body)
  "Execute BODY, silencing any calls to `write-region' within."
  (declare (indent 0))
  `(cl-letf* ((write-region-orig (symbol-function #'write-region))
              ((symbol-function #'write-region)
               (lambda (start end filename &optional append visit lockname
                              mustbenew)
                 (funcall write-region-orig start end filename append 0
                          lockname mustbenew)
                 (set-buffer-modified-p nil)
                 (set-visited-file-modtime)))
              ((symbol-function #'message) #'ignore))
     ,@body))

(defun radian--random-string ()
  "Return a random string designed to be globally unique."
  (md5 (format "%s%s%s%s"
               (system-name) (emacs-pid) (current-time) (random))))

(defun radian--list-of-strings-p (obj)
  "Return non-nil if OBJ is a list of strings."
  (and (listp obj)
       (cl-every #'stringp obj)))

(provide 'boiler-plate)
