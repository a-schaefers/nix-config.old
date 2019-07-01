;;; -*- lexical-binding: t; -*-

(defun disable-all-themes ()
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(which-function-mode -1)

(setq display-time-default-load-average nil
      display-time-24hr-format t)
(display-time-mode 1)

(when (bound-and-true-p mode-line-format)
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval (replace-regexp-in-string "[a-z]+\." "" vc-mode)))))

(setq-default mode-line-mule-info nil
              mode-line-remote nil)

(require 'rich-minority)
(if (bound-and-true-p rich-minority-mode) nil
  (rich-minority-mode 1))
(setf rm-blacklist "")

(require 'sexy-monochrome-theme)
(load-theme 'sexy-monochrome t)
(set-cursor-color "#4870a1")

(custom-set-faces
 '(mode-line ((t (:box nil))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil)))))

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
   '(diff-hl-change ((t (:foreground "#3a81c3" :background "#3a81c3"))))
   '(diff-hl-insert ((t (:foreground "#7ccd7c" :background "#7ccd7c"))))
   '(diff-hl-delete ((t (:foreground "#ee6363" :background "#ee6363"))))
   '(hydra-face-amaranth ((t (:foreground "white" :weight bold))))
   '(hydra-face-blue ((t (:foreground "white" :weight bold))))
   '(hydra-face-pink ((t (:foreground "white" :weight bold))))
   '(hydra-face-red ((t (:foreground "white" :weight bold))))
   '(hydra-face-teal ((t (:foreground "white" :weight bold))))
   '(fringe ((t (:background "black"))))
   '(whitespace-line ((t (:background "gray0" :foreground "dim gray" :underline t))))
   '(header-line ((t (:box nil))))))

(set-face-attribute 'vertical-border nil :foreground "black")

(fringe-mode '(3 . 3))

(custom-set-faces
 '(which-func ((t (:foreground "#3a81c3")))))

(require 'diff-hl)
(setq diff-hl-side "right")
(global-diff-hl-mode 1)

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
    (setq flycheck-highlighting-mode nil)
    (set-face-attribute 'flycheck-warning nil :underline nil)))
