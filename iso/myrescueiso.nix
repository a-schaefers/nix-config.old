# nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=myrescueiso.nix
{config, pkgs, lib, ...}:
let
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
themelios = pkgs.writeScriptBin "themelios" ''
bash <(curl https://raw.githubusercontent.com/a-schaefers/themelios/master/themelios) $@
'';
myDots = pkgs.writeScriptBin "myDots" ''
cat << EOF > ~/.emacs
(setq gc-cons-threshold 100000000
      debug-on-error nil)

(toggle-frame-fullscreen)

(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'server)
(unless (server-running-p)
  (server-start))

(defun spacemacs/alternate-buffer (&optional window)
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defun spacemacs/alternate-window ()
  (interactive)
  (let ((prev-window (get-mru-window nil t t)))
    (unless prev-window (user-error "Last window not found"))nn
    (select-window prev-window)))

(global-set-key (kbd "<home>") 'my-home)
(with-eval-after-load 'exwm
  (exwm-input-set-key (kbd "<home>") 'my-home))
(with-eval-after-load 'erc
  (define-key erc-mode-map (kbd "<home>") 'my-home))

(global-set-key (kbd "<s-tab>") 'ace-window)
(global-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(global-set-key (kbd "<s-backspace>") 'kill-buffer-and-window)
(global-set-key (kbd "s--") 'kill-this-buffer)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-0") 'delete-window)

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

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

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
  "Video player used by eww-open-yt-dl")

(defun eww-open-yt-dl ()
  "Browse youtube videos using the Emacs eww' browser and \"youtube-dl.\"
Specify the video player to use by setting the value of yt-dl-player'"
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

(require 'xelb)
(require 'exwm)
(setq exwm-workspace-number 1)
(require 'exwm-config)
(exwm-config-default)
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

(exwm-input-set-key (kbd "<s-tab>") 'ace-window)
(exwm-input-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(exwm-input-set-key (kbd "<s-backspace>") 'kill-buffer-and-window)
(exwm-input-set-key (kbd "s--") 'kill-this-buffer)
(exwm-input-set-key (kbd "s-1") 'delete-other-windows)
(exwm-input-set-key (kbd "s-2") 'split-window-below)
(exwm-input-set-key (kbd "s-3") 'split-window-right)
(exwm-input-set-key (kbd "s-0") 'delete-window)
(global-unset-key (kbd "s-4"))
(global-unset-key (kbd "s-5"))
(global-unset-key (kbd "s-6"))
(global-unset-key (kbd "s-7"))
(global-unset-key (kbd "s-8"))
(global-unset-key (kbd "s-9"))

(exwm-input-set-key (kbd "<f9>") 'exwm-input-toggle-keyboard)

(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

(require 'desktop-environment)
(desktop-environment-mode)
(exwm-input-set-key
 (kbd "<s-kp-multiply>") 'desktop-environment-toggle-mute)
(exwm-input-set-key
 (kbd "<s-kp-add>") 'desktop-environment-volume-increment)
(exwm-input-set-key
 (kbd "<s-kp-subtract>") 'desktop-environment-volume-decrement)

(defun disable-all-themes ()
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(require 'sexy-monochrome-theme)
(load-theme 'sexy-monochrome t)
(set-cursor-color "#4870a1")
(set-face-attribute 'region nil :background "gray10")
(set-face-attribute 'vertical-border nil :foreground "black")
(custom-set-faces
   '(mode-line ((t (:box nil))))
   '(mode-line-highlight ((t (:box nil))))
   '(mode-line-inactive ((t (:box nil)))))

(setq display-time-default-load-average nil
      display-time-24hr-format nil)
(display-time-mode 1)
(setq-default header-line-format '("%e"
                                   mode-line-modified " "
                                   mode-line-buffer-identification
                                   mode-line-misc-info))
(setq-default mode-line-format nil)

(require 'crux)
(global-set-key [remap move-beginning-of-line] 'crux-move-beginning-of-line)
(global-set-key [remap kill-whole-line] 'crux-kill-whole-line)
(global-set-key (kbd "<C-S-return>") 'crux-smart-open-line-above)
(global-set-key (kbd "<S-return>") 'crux-smart-open-line)
(global-set-key (kbd "C-c r") 'crux-recentf-find-file)
(global-set-key (kbd "C-c R") 'crux-rename-buffer-and-file)
(global-set-key (kbd "C-<backspace>") 'crux-kill-line-backwards)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c D") 'crux-delete-buffer-and-file)
(global-set-key (kbd "C-c K") 'crux-kill-other-buffers)
(global-set-key (kbd "C-c I") (lambda ()
                                (interactive)
                                (if (file-directory-p "/nix-config")
                                    (find-file "/nix-config/external/.emacs.d/init.el")
                                  (crux-find-user-init-file))))
(global-set-key (kbd "C-c s") 'crux-sudo-edit)

(require 'ace-window)
(global-set-key [remap other-window] 'ace-window)
(setq aw-scope 'frame)
(ace-window-display-mode -1)

(defun my-prog-mode-hook ()
  (goto-address-prog-mode 1)
  (whitespace-mode 1))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(global-set-key (kbd "<f5>") 'compile)

(require 'browse-kill-ring)
(global-set-key (kbd "M-y") 'browse-kill-ring)

(require 'webpaste)
(global-set-key (kbd "C-c <print>") 'webpaste-paste-region)

(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))

(require 'paren)
(show-paren-mode 1)

(require 'elec-pair)
(electric-pair-mode 1)

(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq flycheck-display-errors-function nil)
(global-flycheck-mode 1)

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

(defun my-shell ()
  (interactive)
  (if (get-buffer "*shell*")
      (kill-buffer "*shell*"))
  (shell))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(setq explicit-shell-file-name "bash")
(require 'bash-completion)
(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
(defun my-shell-mode-hook()
  (setq-local compile-command
              '((lambda()
                  (save-buffer)
                  (async-shell-command (buffer-file-name))))))
(add-hook 'sh-mode-hook 'my-shell-mode-hook)

(require 'hydra)

(global-set-key (kbd "<menu>") 'caps-hydra/body)

(with-eval-after-load 'exwm ;; in case also using exwm
  (exwm-input-set-key (kbd "<menu>") 'caps-hydra/body))

(defhydra caps-hydra (:exit t)
  "Menu"
  ("#" (my-shell) "sh")
  ("!" (lambda (command)
         (interactive (list (read-shell-command "$ ")))
         (start-process-shell-command command nil command)) "cmd")
  ("i" (irc) "irc")
  ("b" (call-interactively 'eww) "eww")
    ("w" (windows-hydra/body) "win")
  ("<menu>" nil))

;; a nested window mgmt hydra
(require 'transpose-frame)
(defhydra windows-hydra ()
  "Window Management"
  ("v" (flip-frame) "flip-vertically")
  ("h" (flop-frame) "flop-horizontally")
  ("r" (rotate-frame-clockwise) "rotate clockwise")
  ("<left>" (call-interactively 'shrink-window-horizontally)
   "shrink-window-horizontally")
  ("<right>" (call-interactively 'enlarge-window-horizontally)
   "enlarge-window-horizontally")
  ("<down>" (call-interactively 'shrink-window)
   "shrink-window")
  ("<up>" (call-interactively 'enlarge-window)
   "enlarge-window")
  ("q" nil "Quit"))

(require 'aggressive-indent)
(aggressive-indent-global-mode 1)
EOF
'';
in {
imports = [
<nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
<nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
];

networking = {
networkmanager.enable = true;
wireless.enable = false;
nameservers = [ "8.8.8.8" "8.8.4.4" ];
firewall.allowPing = true;
firewall.allowedTCPPorts = [ 22 ];
firewall.allowedUDPPorts = [ 22 ];
};

environment.systemPackages = with pkgs; [
git themelios myDots

(emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
epkgs.better-defaults
epkgs.sexy-monochrome-theme
epkgs.xelb
epkgs.exwm
epkgs.desktop-environment
epkgs.transpose-frame
epkgs.crux
epkgs.flycheck
epkgs.aggressive-indent
epkgs.bash-completion
epkgs.ace-window
epkgs.browse-kill-ring
epkgs.webpaste
epkgs.hydra
])))
gnupg pinentry gnutls (python36.withPackages(ps: with ps; [ certifi ]))
phonon-backend-vlc vlc youtube-dl
wmctrl xclip xsel scrot
shellcheck

nix-prefetch-scripts nixops nix-index
coreutils pciutils
parted
gptfdisk
dosfstools
unzip
zip
lsof tree pstree psmisc
ltrace strace linuxPackages.perf
wget
cryptsetup
efibootmgr
bind
file

pulseaudioFull pavucontrol

udiskie
];

services.udisks2.enable = true;

boot.supportedFilesystems = [ "zfs" ];
boot.zfs.enableUnstable = true;
boot.zfs.requestEncryptionCredentials = true;

sound.enable = true;
nixpkgs.config.pulseaudio = true;
hardware.pulseaudio.enable = true;
hardware.opengl = {
driSupport = true;
};
services.xserver = {
useGlamor = true;
xkbOptions = "ctrl:swap_lalt_lctl, caps:menu";
autoRepeatDelay = 200;
autoRepeatInterval = 25;
enable = true;
layout = "us";
displayManager = {
sddm.enable = true;
sddm.autoLogin.enable = true;
sddm.autoLogin.user = "adam";
};
libinput = {
enable = true;
};
desktopManager = {
xterm.enable = false;
default = "emacs";
session = [ {
manage = "desktop";
name = "emacs";
start = ''
while true; do
until wmctrl -m | grep -q "EXWM" ; do sleep 1 ; done
emacsclient -e "(start-process-shell-command \"udiskie\" nil
                                             \"udiskie -t\")"
break
done &

myDots
emacs &
waitPID=$!
'';
} ];
};
};

security.sudo.wheelNeedsPassword = false;
nix.allowedUsers = [ "root" "@wheel" ];
nix.trustedUsers = [ "root" "@wheel" ];

users.users.adam = {
isNormalUser = true;
createHome = true;
extraGroups = [
"wheel"
"disk"
"audio"
"video"
"systemd-journal"
"networkmanager"
];
initialPassword = "password";
};

i18n = {
consoleUseXkbConfig = true;
consoleFont = "Lat2-Terminus18";
defaultLocale = "en_US.UTF-8";
};

time.timeZone = "America/Los_Angeles";

environment.sessionVariables = {
PAGER = "cat";
EDITOR = "emacsclient";
VISUAL = "emacsclient";
XDG_CURRENT_DESKTOP = "EXWM";
_JAVA_AWT_WM_NONREPARENTING = "1";
};

boot.loader.grub.memtest86.enable = true;
programs = {
mtr.enable = true;
bash.enableCompletion = true;
};

documentation = {
info.enable = true;
man.enable = true;
};

security.sudo.enable = true;

systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];
users.users.root.openssh.authorizedKeys.keys = [
"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCz6zBuWvmgyvkpKqQcUdl7Wt48mvwPtWr+QWwjAF9dLUTJK6Qb7PcDUCjzpeM09cE7rqCXx+dleyxDZXao+ctS6/wgPGdQvcNukSjKxmrmNt407hnyNEIrNhUeHCYhN/cBuuANx90yuJRP6e8cQMIgNBaT6MhNN2ipYy157v++iN5Rm+gEp1CSPRNL9cp7guLGV9VT4T1URJO/b9XE43ca2Y2G6UhZqSAI1NON4jENzw6WW5QmaWNXHJOyKb5ArIHnM4QuyEz8dAfo3oK+l4VQfale0VDlK9k2ugrriLvOaQZt6a756e52cRPc+1r6yQO+YEVvZEGLYl/1cqQwE10OGpUCyFnctrVcot1OCsFnmNmZwgV95yyWAMg8ajACarm1W5Bs4rTLs7UhIgrnkpcAlPsuuWOxpCx9ws3dFMbnbp8O1G/uhKMn0PEotX6ZuAVH40hsMErTRHqnxl6t2rPhiS9sqq6ERFWM8Rci3gSEs+PnTDr5aJ3FGOZ4BeWqVAd4F9V5S1XHXGy4G8vh4Nn2/H7ZxhgHi+F07M0Mt/G8PgUON7qzRcq8V8kxNLpx4uTfNZaWmQWLNZ/hP0ieq++VULYaMCp6tq3kjBX+UKJD0CmBxz89JPyOMALZWoG/sKSasp2JNfdfwhoGS1Djixch3AM66hlT3f7dnmHvH5yE9Q=="
];
}
