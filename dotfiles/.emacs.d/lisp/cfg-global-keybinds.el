;;; -*- lexical-binding: t; -*-

;; All global binds (file is searchable by ";;;;" sections)

;;;; HELM

(use-package helm
  :init (ido-mode -1)
  :config (require 'helm-config)

  ;; don't show nixos .wrapper-binaries
  (require 'helm-external)
  (setq helm-external-commands-list
        (seq-filter (lambda (v) (not (string-match "^\\." v)))
                    (helm-external-commands-list-1 'sort)))

  ;; raise helm-external windows using wmctrl
  (when (executable-find "wmctrl")
    (setq helm-raise-command "wmctrl -xa %s"))

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (setq helm-split-window-inside-p            t
        helm-buffers-fuzzy-matching           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t
        helm-ff-auto-update-initial-value t)

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-command-map (kbd "o")     'helm-occur)
  (define-key helm-command-map (kbd "g")     'helm-do-grep)
  (define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
  (define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-m") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-h f") 'helm-apropos)
  (global-set-key (kbd "C-h r") 'helm-info-emacs)
  (global-set-key (kbd "C-h C-l") 'helm-locate-library)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "C-c C-r") 'helm-resume)
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
  (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

  (helm-mode 1))

(use-package helm-projectile
  :config (helm-projectile-on)
  (setq projectile-completion-system 'helm))

(use-package helm-descbinds
  :config (helm-descbinds-mode))

(use-package helm-swoop :config
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

  (setq helm-swoop-use-fuzzy-match t))

(use-package helm-emms :defer t)

(use-package helm-rg :after rg :defer t)
(use-package helm-ag :after ag :defer t)

;;;; HYDRA
(use-package ace-window
  :init (setq aw-scope 'frame)
  :bind ("<s-return>" . ace-window)
  :config (ace-window-display-mode -1))
(use-package transpose-frame :defer t)
(use-package operate-on-number :defer t)
(use-package goto-chg :defer t)
(use-package hydra :defer t
  ;; :init (setq hydra-lv nil) ; use minibuffer for hints
  :bind (("<f1>" . f1-hydra/body)
         ("<menu>" . caps-hydra/body)) ; on Caps Lock via setxkbmap -option caps:menu
  :config

  ;; caps hydra
  (defhydra caps-hydra (:exit nil)
    "NAVI-MODE"
    ("<menu>" nil)

    ("=" ((lambda ()
            (start-process-shell-command "notify-send" nil "notify-send Smarparens-Strict-Mode Toggled")
            (call-interactively #'smartparens-strict-mode))))

    ("C-g" (keyboard-quit))
    ("g" (keyboard-quit))
    ("SPC" (call-interactively #'set-mark-command))
    ("C-SPC" (call-interactively #'set-mark-command))
    ("C-n" (next-line))
    (";" (call-interactively #'goto-last-change))
    ("," (call-interactively #'goto-last-change-reverse))
    ("C-p" (previous-line))
    ("C-f" (forward-char))
    ("C-b" (backward-char))
    ("C-v" (scroll-up-command))
    ("v" (scroll-up-command))
    ("M-v" (scroll-down-command))
    ("V" (scroll-down-command))

    ("j" (dumb-jump-go))
    ("k" (dumb-jump-back))
    ("l" (dumb-jump-quick-look))

    ("<" (beginning-of-buffer))
    (">" (end-of-buffer))
    ("\[" (backward-paragraph))
    ("\]" (forward-paragraph))
    ("s-f" (sp-forward-symbol))
    ("M-f" (forward-word))
    ("s-b" (sp-backward-symbol))
    ("M-b" (backward-word))

    ("e" (sp-forward-sexp))
    ("a" (sp-backward-sexp))
    ("f" (sp-down-sexp))
    ("b" (sp-up-sexp))
    ("m" (call-interactively #'magit-status))
    ("n" (sp-next-sexp))
    ("p" (sp-previous-sexp))
    ("s" (sp-select-next-thing))
    ("S" (sp-select-previous-thing))

    ("B" (helm-buffers-list))
    ("E" ((lambda ()
            (flycheck-mode 1)
            (flycheck-list-errors))))
    ("P" (projectile-commander))
    ("F" (call-interactively #'helm-find-files))
    ("D" (dired (helm-current-directory)))
    ("M" (call-interactively #'magit-status))

    ("/" (helm-swoop))
    ("+" (operate-on-number-at-point)))

  ;; esc/f1 hydra
  (defhydra f1-hydra (:exit t)
    "Main Menu"
    ("c" (call-interactively #'display-time-mode) "clock")
    ("o" (org-hydra/body) "org")
    ("i" (init/body) "init")
    ("g" (gist/body) "gists")
    ("r" (call-interactively #'helm-run-external-command) "run-external")
    ("s" (my-shell-refresh))
    ("t" (my-ansi-term) "term")
    ("w" (windows-hydra/body) "windows")
    ("<f1>" nil))

  ;; subhydras
  (defhydra gist (:exit t)
    "gist and webpaste"
    ("p" (call-interactively #'webpaste-paste-region) "webpaste region")
    ("P" (webpaste-paste-buffer) "webpaste buffer")
    ("q" nil "Quit"))

  (defhydra init (:exit t)
    "init.el"
    ("i" (my-find-user-init-file) "init.el edit")
    ("q" nil "Quit"))

  (defhydra windows-hydra ()
    "Window Management"
    ("a" (call-interactively #'ace-window) "ace")
    ("v" (flip-frame) "flip-vertically")
    ("h" (flop-frame) "flop-horizontally")
    ("r" (rotate-frame-clockwise) "rotate clockwise")
    ("R" (rotate-frame-anticlockwise) "rotate anti-clockwise")
    ("t" (transpose-frame) "transpose")
    ("w" (call-interactively #'exwm-workspace-move-window) "exwm move win to workspace")
    ("<left>" (call-interactively #'shrink-window-horizontally) "shrink-window-horizontally")
    ("<right>" (call-interactively #'enlarge-window-horizontally) "enlarge-window-horizontally")
    ("<down>" (call-interactively #'shrink-window) "shrink-window")
    ("<up>" (call-interactively #'enlarge-window) "enlarge-window")
    ("<s-up>" (windmove-up) "move up")
    ("<s-down>" (windmove-down) "move down")
    ("<s-right>" (windmove-right) "move right")
    ("<s-left>" (windmove-left) "move left")
    ("0" (delete-window) "")
    ("s-0" (delete-window) "")
    ("1" (delete-other-windows) "")
    ("s-1" (delete-other-windows) "")
    ("2" (split-window-below) "")
    ("s-2" (split-window-below) "")
    ("3" (split-window-right) "")
    ("s-3" (split-window-right) "")
    ("q" nil "Quit"))

  (defhydra emms-hydra (:exit t)
    "Emacs Multimedia System"
    ("h" helm-emms "helm-emms")
    ("f" emms-play-file "play file")
    ("d" emms-play-directory "play directory")
    ("p" emms "playlist")
    ("q" nil "Quit"))

  (defhydra org-hydra (:exit t)
    "Org"
    ("a" (org-agenda) "Agenda")
    ("c" (clock-hydra/body) "Clock")
    ("q" nil "Quit"))

  (defhydra clock-hydra (:color blue)
    "
    ^
    ^Clock^             ^Do^
    ^─────^─────────────^──^─────────
    _q_ quit            _c_ cancel
    ^^                  _d_ display
    ^^                  _e_ effort
    ^^                  _i_ in
    ^^                  _g_ goto
    ^^                  _o_ out
    ^^                  _r_ report
    ^^                  ^^
    "
    ("q" nil)
    ("c" org-clock-cancel :color pink)
    ("d" org-clock-display)
    ("e" org-clock-modify-effort-estimate)
    ("i" org-clock-in)
    ("g" org-clock-goto)
    ("o" org-clock-out)
    ("r" org-clock-report)))


;;;; CRUX

(use-package crux
  :config
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key [(shift return)] 'crux-smart-open-line)
  (global-set-key [(control shift return)] 'crux-smart-open-line-above)
  (global-set-key (kbd "s-c") 'crux-duplicate-and-comment-current-line-or-region)
  (global-set-key (kbd "s-d") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C--") 'crux-kill-whole-line)
  (global-set-key (kbd "s-u") 'crux-kill-line-backwards)
  (global-set-key (kbd "s-D") 'crux-delete-file-and-buffer)
  (global-set-key (kbd "s-r") 'crux-rename-buffer-and-file))

;;;; SMARTPARENS

(use-package smartparens
  :config (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; smart curly braces
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))

  ;; more smartparens binds
  (define-key smartparens-mode-map (kbd "s-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "s-f") 'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "s-b") 'sp-backward-symbol)

  ;; working with pairs
  (define-key smartparens-mode-map (kbd "s-)") 'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "s-]") 'sp-wrap-square)
  (define-key smartparens-mode-map (kbd "s-}") 'sp-wrap-curly)

  (define-key smartparens-mode-map (kbd "s-9") 'sp-backward-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "s-(") 'sp-backward-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "s-[") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "s-{") 'sp-unwrap-sexp))

;;;; GENERAL

;; prefer M-SPC and default C-SPC do the same things
(global-set-key (kbd "M-g") 'keyboard-quit)
(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key (kbd "s-w") 'save-buffer)

(global-set-key (kbd "s-/") 'winner-undo)
(global-set-key (kbd "s-?") 'winner-redo)

;; misc
(global-set-key (kbd "<f5>") 'compile)

(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "s-;") 'comment-line)

(global-set-key (kbd "<home>") 'my-custom-startup)
(with-eval-after-load 'erc
  (define-key erc-mode-map (kbd "<home>") 'my-custom-startup))

;; buffers windows and splits
(global-set-key (kbd "<s-return>") 'ace-window)
(global-set-key (kbd "C-x <tab>") 'spacemacs/alternate-buffer)
(global-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<C-tab>") 'spacemacs/alternate-buffer))
(global-set-key (kbd "<s-backspace>") 'kill-buffer-and-window)
(global-set-key (kbd "s--") 'kill-this-buffer)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-0") 'delete-window)

(provide 'cfg-global-keybinds)
