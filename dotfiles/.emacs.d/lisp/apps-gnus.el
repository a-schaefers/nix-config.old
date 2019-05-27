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
  (setq smtpmail-smtp-server "email-smtp.us-west-2.amazonaws.com"
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
      ad-do-it)))

(provide 'apps-gnus)
