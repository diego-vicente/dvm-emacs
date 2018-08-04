(setq user-mail-address "mail@diego.codes"
      user-full-name "Diego Vicente")

(setq gnus-select-method
      '(nnimap "localhost"
               (nnimap-address "127.0.0.1")
               (nnimap-server-port 1143)
               (nnimap-stream plain)))

(setq send-mail-function           'smtpmail-send-it
      message-send-mail-function   'smtpmail-send-it
      smtpmail-default-smtp-server "127.0.0.1"
      smtpmail-smtp-server         "127.0.0.1"
      smtpmail-smtp-service        1025)

(define-key gnus-summary-mode-map "d"
  'gnus-summary-mark-as-expirable)

(use-package gnus-desktop-notify
  :ensure t
  :after gnus
  :config (gnus-desktop-notify-mode))

(gnus-demon-add-scanmail)
