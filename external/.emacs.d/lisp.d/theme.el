;;; -*- lexical-binding: t; -*-

;; font
(set-face-attribute 'default nil :font "Noto Sans Mono-15")

;; seamlessly switch themes
(defun disable-all-themes ()
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

;;  theme
(require 'sexy-monochrome-theme)
(load-theme 'sexy-monochrome t)
(with-eval-after-load 'sexy-monochrome-theme
  ;; my theme changes
  (set-cursor-color "#4870a1")
  (set-face-attribute 'region nil :background "gray10")
  (set-face-attribute 'vertical-border nil :foreground "black")
  (custom-set-faces
   '(mode-line ((t (:box nil))))
   '(mode-line-highlight ((t (:box nil))))
   '(mode-line-inactive ((t (:box nil)))))

  ;; uninvasive diff-hl
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

  ;; fringe settings
  (fringe-mode '(3 . 3))
  (with-eval-after-load 'diff-hl
    (setq diff-hl-side "right")
    (global-diff-hl-mode 1))

  ;; uninvasive flycheck
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
      (set-face-attribute 'flycheck-warning nil :underline nil))))


;; minimalist modeline
(setq display-time-default-load-average nil
      display-time-24hr-format nil)
(display-time-mode 1)
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))
(setq-default mode-line-format
              '((:eval (simple-mode-line-render
                        ;; left
                        (format-mode-line "[%*] %b %l:%c")
                        ;; right
                        (format-mode-line 'mode-line-misc-info)))))
(with-eval-after-load 'emms-setup
  (emms-playing-time -1)
  (emms-mode-line -1))
