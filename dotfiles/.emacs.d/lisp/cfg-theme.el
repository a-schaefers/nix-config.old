;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------MODELINE

;; show modeline time
(setq display-time-default-load-average nil
      display-time-24hr-format t)
(display-time-mode 1)

;; hide version control from modeline
(defun hide-vc ()
  (when (bound-and-true-p mode-line-format)
    (setcdr (assq 'vc-mode mode-line-format)
            '((:eval (replace-regexp-in-string "[a-z]+\." "" vc-mode))))))
(hide-vc)

;; hide mule info from modeline
(setq-default mode-line-mule-info nil)

;; hide mode line remote
(setq-default mode-line-remote nil)

;; hide modeline active function name
(which-function-mode -1)

;; hide all minor modes from modeline
(use-package rich-minority
  :init (if (bound-and-true-p rich-minority-mode) nil
          (rich-minority-mode 1))
  :config (setf rm-blacklist ""))

;;------------------------------------------------------------------THEME

(use-package sexy-monochrome-theme
  :config (load-theme 'sexy-monochrome t)
  (global-hl-line-mode -1)

  ;; font
  (setq my-font "Hack-18")
  (set-face-attribute 'default nil :font my-font)

  ;; theme mods
  (set-cursor-color "#4870a1")

  (with-eval-after-load 'company
    (require 'color)
    (let ((bg (face-attribute 'default :background)))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
       `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
       `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

  (with-eval-after-load 'diff-hl
    (custom-set-faces
     (custom-set-faces
      '(diff-hl-change ((t (:foreground "#3a81c3" :background "#3a81c3"))))
      '(diff-hl-insert ((t (:foreground "#7ccd7c" :background "#7ccd7c"))))
      '(diff-hl-delete ((t (:foreground "#ee6363" :background "#ee6363")))))

     '(hydra-face-amaranth ((t (:foreground "white" :weight bold))))
     '(hydra-face-blue ((t (:foreground "white" :weight bold))))
     '(hydra-face-pink ((t (:foreground "white" :weight bold))))
     '(hydra-face-red ((t (:foreground "white" :weight bold))))
     '(hydra-face-teal ((t (:foreground "white" :weight bold))))))
  (custom-set-faces
   '(fringe ((t (:background "black"))))
   '(whitespace-line ((t (:background "gray0" :foreground "dim gray" :underline t))))
   '(header-line ((t (:box nil)))))
  (set-face-attribute 'vertical-border nil :foreground "black")

  ;; left-fringe is flycheck, right-fringe is diff-hl
  (fringe-mode '(3 . 3)))

;;------------------------------------------------------------------MISC.

;; git add/remove/changed line highlighting on the right-fringe
(use-package diff-hl
  :config
  (setq diff-hl-side "right")
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;------------------------------------------------------------------ANNOYANCES

(blink-cursor-mode -1) ; disable the  blinking cursor

(setq visible-bell nil); disable the frame flash

;; flycheck show minimal "|" in left-fringe
(with-eval-after-load 'flycheck
  (progn
    (define-fringe-bitmap 'flycheck-fringe-bitmap-nil
      (vector #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111
              #b00111111))
    (flycheck-define-error-level 'error
      :severity 100
      :compilation-level 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-nil
      :fringe-face 'flycheck-fringe-error
      :error-list-face 'flycheck-error-list-error)
    (flycheck-define-error-level 'warning
      :severity 100
      :compilation-level 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-nil
      :fringe-face 'flycheck-fringe-error
      :error-list-face 'flycheck-error-list-error)
    (flycheck-define-error-level 'info
      :severity 100
      :compilation-level 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-nil
      :fringe-face 'flycheck-fringe-error
      :error-list-face 'flycheck-error-list-error)

    ;; remove flycheck underlined text
    (setq flycheck-highlighting-mode nil)
    (set-face-attribute 'flycheck-warning nil :underline nil)))

(provide 'cfg-theme)
