;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------ANNOYANCES

(blink-cursor-mode -1) ; disable the  blinking cursor
(setq visible-bell nil); disable the frame flash

;;------------------------------------------------------------------MODELINE

;; hide mule info from modeline
(setq-default mode-line-mule-info nil)

;; hide mode line remote
(setq-default mode-line-remote nil)

;; hide modeline time
(setq display-time-default-load-average nil
      display-time-24hr-format t)
(display-time-mode -1)

;; hide modeline active function name
(which-function-mode -1)

;; hide all minor modes from modeline
(use-package rich-minority
  :init (if (bound-and-true-p rich-minority-mode) nil
          (rich-minority-mode 1))
  :config (setf rm-blacklist ""))

;;------------------------------------------------------------------THEMES
(use-package zerodark-theme
  :init
  ;; don't show column
  (defvar zerodark-modeline-position '(:eval (propertize " %l %p " 'face (if (zerodark--active-window-p)
                                                                             'zerodark-buffer-position-face
                                                                           'mode-line-inactive)))
    "Mode line construct for displaying the position in the buffer.")
  :config
  (load-theme 'zerodark t)
  (zerodark-setup-modeline-format)

  (defun my-simple-diff-hl-theme ()
    (with-eval-after-load 'diff-hl
      (custom-set-faces
       '(diff-hl-change ((t (:foreground "#3a81c3" :background "#3a81c3"))))
       '(diff-hl-insert ((t (:foreground "#7ccd7c" :background "#7ccd7c"))))
       '(diff-hl-delete ((t (:foreground "#ee6363" :background "#ee6363")))))))

  (global-hl-line-mode -1)

  ;; font
  (setq my-font "Source Code Pro-18")
  (set-face-attribute 'default nil :font my-font)

  ;; left-fringe is flycheck, right-fringe is diff-hl
  (fringe-mode '(2 . 4)))

;;------------------------------------------------------------------MISC.

;; bootstrap all the icons font
(use-package all-the-icons :defer t :init
  (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
    (all-the-icons-install-fonts t)))

;; all the icons for dired
(use-package all-the-icons-dired :defer t :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; git add/remove/changed line highlighting on the right-fringe
(use-package diff-hl
  :config
  (setq diff-hl-side "right")
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(with-eval-after-load 'flycheck
  (progn
    ;; flycheck show minimal "|" in left-fringe
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
