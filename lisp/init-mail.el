;;; init-mail.el --- Configure Emacs as an email client.
;;; Commentary:

;;; This file includes basic Emacs configuration to be used as a plain-text
;;; email client. This is the perfect use case for my personal account, which I
;;; use mostly for mailing lists and such.

;;; Code:

;; mu4e is my favorite email client
(use-package mu4e
  :after shr
  :config
  (setq user-full-name "Diego Vicente"
        user-mail-address "mail@diego.codes"
        mu4e-compose-reply-to-address "mail@diego.codes"
        mail-user-agent 'mu4e-user-agent
        ;; Basic settings
        mu4e-completing-read-function #'ivy-completing-read
        mu4e-view-prefer-html t  ; managed by shr
        ;; Directory settings
        mu4e-maildir (expand-file-name "~/docs/maildir")
        mu4e-attachment-dir (expand-file-name "~/docs/downloads")
        ;; Sync mail using mbsync
        mu4e-get-mail-command "mbsync -a"
        ;; Folder settings
        mu4e-sent-folder "/personal/Sent"
        mu4e-refile-folder "/personal/Archive"
        mu4e-drafts-folder "/personal/Drafts"
        mu4e-trash-folder "/personal/Trash"
        ;; Shortcuts - to use with `j`
        mu4e-maildir-shortcuts
        '((:maildir "/personal/Inbox" :key ?i)
          (:maildir "/personal/Sent"  :key ?s))
        ;; Configure msmtp for outgoing email
        sendmail-program "msmtp"
        send-mail-function 'smtpmail-send-it
        message-sendmail-function 'message-send-mail-with-sendmail
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        smtpmail-smtp-server "smtp.migadu.com"
        smtpmail-smtp-user "mail@diego.codes"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        smtpmail-default-smtp-server "smtp.migadu.com")

  ;; Define a launcher in the SPC SPC prefix
  (leader-def
    "SPC m" 'mu4e))

;; mu4e-alert manages mode-line counter and new mail notifications
(use-package mu4e-alert
  :ensure t
  :after mu4e
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'libnotify)
  :hook (after-init . mu4e-alert-enable-notifications))

;; Configure mu4e in a per-column basis
;; TODO: requires at least mu4e 1.6.0
;; (use-package mu4e-column-faces
;;   :ensure t
;;   :after mu4e
;;   :config (mu4e-column-faces-mode))


(provide 'init-mail)
;;; init-mail.el ends here
