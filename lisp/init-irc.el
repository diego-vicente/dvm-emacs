;;; init-irc.el --- Configure IRC chats for Emacs
;;; Commentary:

;;; This file includes a basic IRC configuration using circe, a great
;;; IRC client for Emacs. Most of this configuration is taken from the
;;; setup in doom-emacs for IRC.

;;; Code:

(use-package circe
  :ensure t
  :commands circe
  :config
  (defvar dvm/irc-left-padding 13
    "Size of the left margin in the IRC chats.")

  (defsubst dvm/irc-pad (left right)
    "Format two parts of a message with a padding in between."
    (format (format "%%%ds | %%s" dvm/irc-left-padding)
            (concat "*** " left) right))

  (setq circe-default-quit-message nil
        circe-default-part-message nil
        circe-use-cycle-completion t
        circe-reduce-lurker-spam t

        ;; Define the format to be left aligned
        circe-format-say (format "{nick:+%ss} │ {body}" dvm/irc-left-padding)
        circe-format-self-say circe-format-say
        circe-format-action (format "{nick:+%ss} * {body}" dvm/irc-left-padding)
        circe-format-self-action circe-format-action
        circe-format-server-notice
        (let ((left "-Server-")) (concat (make-string (- dvm/irc-left-padding (length left)) ? )
                                         (concat left " _ {body}")))
        circe-format-notice
        (format "{nick:%ss} _ {body}" dvm/irc-left-padding)
        circe-format-server-topic
        (dvm/irc-pad "Topic" "{userhost}: {topic-diff}")
        circe-format-server-join-in-channel
        (dvm/irc-pad "Join" "{nick} ({userinfo}) joined {channel}")
        circe-format-server-join
        (dvm/irc-pad "Join" "{nick} ({userinfo})")
        circe-format-server-part
        (dvm/irc-pad "Part" "{nick} ({userhost}) left {channel}: {reason}")
        circe-format-server-quit
        (dvm/irc-pad "Quit" "{nick} ({userhost}) left IRC: {reason}]")
        circe-format-server-quit-channel
        (dvm/irc-pad "Quit" "{nick} ({userhost}) left {channel}: {reason}]")
        circe-format-server-rejoin
        (dvm/irc-pad "Re-join" "{nick} ({userhost}), left {departuredelta} ago")
        circe-format-server-netmerge
        (dvm/irc-pad "Netmerge" "{split}, split {ago} ago (Use /WL to see who's still missing)")
        circe-format-server-nick-change
        (dvm/irc-pad "Nick" "{old-nick} ({userhost}) is now known as {new-nick}")
        circe-format-server-nick-change-self
        (dvm/irc-pad "Nick" "You are now known as {new-nick} ({old-nick})")
        circe-format-server-nick-change-self
        (dvm/irc-pad "Nick" "{old-nick} ({userhost}) is now known as {new-nick}")
        circe-format-server-mode-change
        (dvm/irc-pad "Mode" "{change} on {target} by {setter} ({userhost})")
        circe-format-server-lurker-activity
        (dvm/irc-pad "Lurk" "{nick} joined {joindelta} ago")

        circe-network-options
        `(("Libera"
           :host "irc.libera.chat"
           :port 6697
           :tls t
           :nick "dvicente"
           :sasl-username "dvicente"
           :sasl-password ,(dvm/read-gpg-secret
                            (expand-file-name "irc.asc"
                                              dvm/password-directory))
           :channels ("#emacs" "#sr.ht" "#nixos")))))


(provide 'init-irc)
;;; init-irc.el ends here
