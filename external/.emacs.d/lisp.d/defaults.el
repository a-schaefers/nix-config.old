;;; -*- lexical-binding: t; -*-

;; swapping left-ctl with left-alt is the best way to "ergo" Emacs.
;; (start-process-shell-command "setxkbmap" nil
;;                              "setxkbmap -option ctrl:swap_lalt_lctl")

(require 'better-defaults)

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      inhibit-startup-screen t
      initial-major-mode 'emacs-lisp-mode)

(defun my-home ()
  (interactive)
  (when (get-buffer "*scratch*")
    (kill-buffer "*scratch*"))
  (if (get-buffer "*shell*")
      (switch-to-buffer "*shell*")
    (shell))
  (delete-other-windows)
  (cd "~/"))

(add-hook 'after-init-hook 'my-home)

(add-hook 'text-mode-hook 'goto-address-mode)

(global-set-key (kbd "<C-kp-add>") 'text-scale-increase)
(global-set-key (kbd "<C-kp-subtract>") 'text-scale-decrease)

(defun spacemacs/alternate-buffer (&optional window)
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(global-set-key (kbd "C-x TAB") 'spacemacs/alternate-buffer)
(global-set-key (kbd "C-x <tab>") 'spacemacs/alternate-buffer)

(global-set-key (kbd "<home>") 'my-home)
(with-eval-after-load 'exwm
  (exwm-input-set-key (kbd "<home>") 'my-home))

(with-eval-after-load 'erc
  (define-key erc-mode-map (kbd "<home>") 'my-home))

(delete-selection-mode 1)
(blink-cursor-mode -1)
(setq visible-bell nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default fill-column 80)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(setq vc-follow-symlinks t)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'dired-x)
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(setq large-file-warning-threshold '100000000000000)
(require 'openwith)
(setq openwith-associations
      (list
       (list (openwith-make-extension-regexp
              '("mpg" "mpeg" "mp3" "mp4"
                "avi" "wmv" "wav" "mov" "flv"
                "ogm" "ogg" "mkv"))
             "vlc"
             '(file))
       (list (openwith-make-extension-regexp
              '("xbm" "pbm" "pgm" "ppm" "pnm"
                "png" "gif" "bmp" "tif" "jpeg" "jpg"))
             "feh"
             '(file))
       (list (openwith-make-extension-regexp
              '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
             "libreoffice"
             '(file))))
(openwith-mode 1)

(setq tab-always-indent 'complete)

(setq tramp-default-method "ssh")
(setq tramp-copy-size-limit nil)

(require 'recentf)
(setq recentf-max-saved-items 500
      recentf-max-menu-items 15)
(recentf-mode +1)

(require 'midnight)
(setq midnight-period 7200)
(midnight-mode 1)

(require 'winner)
(winner-mode 1)

;; use eww as the default web browser
(require 'eww)
(setq browse-url-browser-function 'eww-browse-url)

(defun my-external-browser (url)
  (start-process-shell-command "chrome" nil (concat "chrome " url)))

;; opened by eww with "&" key
(setq shr-external-browser 'my-external-browser)

(defvar yt-dl-player "vlc"
  "Video player used by `eww-open-yt-dl'")

(defun eww-open-yt-dl ()
  "Browse youtube videos using the Emacs `eww' browser and \"youtube-dl.\"
Specify the video player to use by setting the value of `yt-dl-player'"
  (interactive)
  (if (executable-find "youtube-dl")
      (progn
        (eww-copy-page-url)
        (start-process-shell-command "youtube-dl" nil
                                     (concat "youtube-dl -o - " (nth 0 kill-ring) " - | " yt-dl-player " -")))
    (progn
      (setq xbuff (generate-new-buffer "*youtube-dl not found*"))
      (with-output-to-temp-buffer xbuff
        (print "Ensure youtube-dl is installed on the system and try again...")))))

;; browse youtube videos from eww  with "^" key
(define-key eww-mode-map (kbd "^") 'eww-open-yt-dl)
