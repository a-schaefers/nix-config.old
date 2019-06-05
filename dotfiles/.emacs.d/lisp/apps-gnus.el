;;; -*- lexical-binding: t; -*-

;; use my custom address inserter
(setq my-contacts-file "~/Private/contacts.el")
(when (file-exists-p my-contacts-file)
  (load-file my-contacts-file))

;; e.g. dummy address book (key . value) list
;; (setq my-contact-list '(
;;                         (adam . sch@efers.org)
;;                         (nick . nick@nick.com)
;;                         (john . john@doe.com)
;;                         ))

(setq my-contact-keys (cl-loop for (key . value) in my-contact-list
                               collect key))

(defun my-insert-contact ()
  "Insert an email address from `my-contact-list' to the current buffer."
  (interactive)
  (let ((item my-contact-keys))
    ;; support helm and ido
    (if (fboundp 'helm)
        (fset 'helm-or-ido-read 'helm-comp-read)
      (fset 'helm-or-ido-read 'ido-completing-read))

    ;; interactive menu + convert chosen item (key) from string to data
    (setq interactive-chosen-key (intern (helm-or-ido-read "Contact Name:" item)))
    ;; match key to list and get associated email (value), convert back to string
    (setq email (format "%s" (cdr (assq interactive-chosen-key my-contact-list))))

    ;; output email address to buffer
    (princ email (current-buffer))))

(with-eval-after-load 'gnus
  ;; don't screw up window layouts
  (setq gnus-use-full-window nil)

  ;; don't use ~/.gnus, use gnus file with my other lisp files instead.
  (setq gnus-site-init-file "~/.emacs.d/lisp/apps-gnus.el")

  ;; keep clutter out of $HOME
  (setq gnus-save-newsrc-file nil)
  (setq gnus-startup-file "~/.emacs.d/.newsrc")
  (setq message-directory "~/.emacs.d/mail/")
  (setq gnus-directory "~/.emacs.d/news/")
  (setq nnfolder-directory "~/.emacs.d/mail/archive")
  (setq nndraft-directory "~/.emacs.d/mail/drafts")

  ;; yes read dribble file on startup question
  (setq gnus-always-read-dribble-file t)

  ;; imap
  (setq gnus-select-method '(nnimap "gmail"
                                    (nnimap-address "imap.gmail.com")
                                    (nnimap-server-port 993)
                                    (nnimap-server-port "imaps")
                                    (nnimap-stream ssl)
                                    (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                                    (nnmail-expiry-wait immediate)))

  ;; smtp
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-debug-info t smtpmail-debug-verb t
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
        gnus-message-archive-method '(nnimap "gmail")
        gnus-message-archive-group "nnimap+gmail:[Gmail]/Sent Mail"
        gnus-gcc-mark-as-read t)

  ;; html mail no thanks
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")
    (setq mm-automatic-display (remove "text/html" mm-automatic-display)))


  ;; misc settings from spacemacs that I am in the habit of using.
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (setq gnus-visible-headers
        "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")
  (setq gnus-sorted-header-list
        '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
          "^Subject:" "^Date:" "^Gnus"))
  (setq-default
   gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
   gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
   gnus-group-line-format "%M%S%p%P%5y:%B %G\n";;"%B%(%g%)"
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
   gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
   gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
   gnus-sum-thread-tree-false-root ""
   gnus-sum-thread-tree-indent " "
   gnus-sum-thread-tree-leaf-with-other "├► "
   gnus-sum-thread-tree-root ""
   gnus-sum-thread-tree-single-leaf "╰► "
   gnus-sum-thread-tree-vertical "│"
   gnus-article-browse-delete-temp t
   gnus-treat-strip-trailing-blank-lines 'last
   gnus-keep-backlog 'nil
   gnus-summary-display-arrow nil
   gnus-mime-display-multipart-related-as-mixed t
   gnus-auto-select-first nil
   smiley-style 'medium
   gnus-keep-backlog '0)

  ;; Don't spam the minibuffer! "No news is good news"
  (setq gnus-no-groups-message "")

  ;; Have Gnus check for new mail every ~5 minutes if idle
  (gnus-demon-add-handler 'gnus-demon-scan-news 5 t)

  ;; work around issue of gnus demon hang emacs upon connection loss
  ;; (hang for only 3 seconds instead of indefinitely.)
  (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
    "3 second Timeout for Gnus."
    (with-timeout
        (3 (message "Gnus timed out."))
      ad-do-it))

  ;; gnus-notify allows you to select which groups you want to include in the report;
  ;; it does this via a „group parameter“ you have to add manually.
  ;; To do it: use G p in the group buffer, then add (modeline-notify t) to the list there;
  ;; if it were the only property, it would look like this: ((modeline-notify t))

  ;; Author: Mark Triggs <mark@dishevelled.net>
  ;;
  ;; Contributions from: Frederic Couchet <fcouchet AT april.org>

  ;; This file is free software; you can redistribute it and/or modify
  ;; it under the terms of the GNU General Public License as published by
  ;; the Free Software Foundation; either version 2, or (at your option)
  ;; any later version.

  ;; This file is distributed in the hope that it will be useful,
  ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
  ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ;; GNU General Public License for more details.

  ;; You should have received a copy of the GNU General Public License
  ;; along with GNU Emacs; see the file COPYING.  If not, write to
  ;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  ;; Boston, MA 02111-1307, USA.

  ;; Commentary:

  ;; This code provides modeline notification of when certain groups contain
  ;; unread messages. Groups for whom unread messages should be indicated are
  ;; chosen by setting a group parameter.

  ;; Clicking on a group in the modeline will enter that group and view the new
  ;; message.

  ;; Code:

  (require 'cl-lib)

  (defvar gnus-notify-show-unread-counts t
    "If true, show the number of unread messages in the modeline in addition to shortened group names.")


  (when (fboundp 'gnus-define-group-parameter)
    (gnus-define-group-parameter
     modeline-notify
     :type bool
     :parameter-type '(const :tag "Notify of new messages for this group." t)
     :parameter-document "\
If this is set, the name of this group will be placed on the modeline when it
contains new messages"))

  (defvar gnus-mst-display-new-messages "")
  (defvar gnus-mst-notify-groups '())
  (defvar gnus-notify-jump-to-group-hook '()
    "This hook is invoked before jumping to a gnus group with unread messages.
  Each hook should take a single argument - the GROUP to be selected")


  (add-hook 'gnus-exit-gnus-hook
            (lambda ()
              (setq gnus-mst-display-new-messages "")))


  (defun gnus-mst-notify-modeline-form ()
    gnus-mst-display-new-messages)


  (if (featurep 'xemacs)
      (unless (member 'gnus-mst-display-new-messages global-mode-string)
        (if (null global-mode-string)
            (setq global-mode-string '("" gnus-mst-display-new-messages))
          (setq global-mode-string
                (append global-mode-string
                        '(gnus-mst-display-new-messages)))))
    (unless (member '(:eval (gnus-mst-notify-modeline-form)) global-mode-string)
      (setq global-mode-string
            (append global-mode-string
                    (list '(:eval (gnus-mst-notify-modeline-form)))))))


  (defun gnus-mst-notify-shorten-group-name (group)
    "shorten the group name to make it better fit on the modeline"
    (let ((name (if (string-match ":" group)
                    (cadr (split-string group "[:]"))
                  group)))
      (mapconcat 'identity
                 (mapcar
                  (lambda (segment)
                    (string (elt segment 0)))
                  (split-string name "[\\./]"))
                 ".")))


  (defun gnus-mst-notify-update-modeline ()
    "Update the modeline to show groups containing new messages"
    (if gnus-mst-notify-groups
        (setq gnus-mst-display-new-messages
              (append (list " [m: ")
                      (cl-maplist
                       (lambda (sublist)
                         (let ((group (car sublist))
                               (map (make-sparse-keymap)))
                           (define-key map [mode-line mouse-1]
                             `(lambda ()
                                (interactive)
                                (run-hook-with-args
                                 'gnus-notify-jump-to-group-hook ,group)
                                (gnus-group-read-group nil nil ,group)))
                           (cl-list*
                            (list ':propertize
                                  (if gnus-notify-show-unread-counts
                                      (format "[%s %s]"
                                              (gnus-mst-notify-shorten-group-name
                                               (car sublist))
                                              (gnus-group-unread (car sublist)))
                                    (format "%s"
                                            (gnus-mst-notify-shorten-group-name
                                             (car sublist))))
                                  'face 'bold
                                  'keymap map
                                  'help-echo "Visit this group")
                            (if (cdr sublist)
                                (list ", ")
                              nil))))
                       gnus-mst-notify-groups)
                      (list "] ")))
      (setq gnus-mst-display-new-messages "")))


  (defun gnus-mst-notify-group (group)
    "Add notification for this group"
    (unless (member group gnus-mst-notify-groups)
      (add-to-list 'gnus-mst-notify-groups group)
      (gnus-mst-notify-update-modeline)))


  (defun gnus-mst-show-groups-with-new-messages (&rest ignored)
    (interactive)
    (setq gnus-mst-notify-groups '())
    (gnus-mst-notify-update-modeline)
    (mapc #'(lambda (g)
              (let* ((group (car g))
                     (unread (gnus-group-unread group)))
                (when (and (cdr (assoc 'modeline-notify
                                       (gnus-group-find-parameter group)))
                           (and (numberp unread) (> unread 0)))
                  (gnus-mst-notify-group group))))
          gnus-newsrc-alist))


  (add-hook 'gnus-after-getting-new-news-hook
            'gnus-mst-show-groups-with-new-messages)


  (add-hook 'gnus-summary-exit-hook
            'gnus-mst-show-groups-with-new-messages)

  )

(provide 'apps-gnus)
