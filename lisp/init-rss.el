;;; init-rss.el --- Configure RSS reader for Emacs
;;; Commentary:

;;; Configure Emacs to be used as an RSS reader using elfeed. This file
;;; includes the feed definition, automatic tagging and the basic web view
;;; configuration.

;;; Code:

(use-package elfeed
  :ensure t
  :after shr
  :config
  (setq elfeed-feeds
        '(("https://christine.website/blog.rss" blog)
          ("https://www.feoh.org/feeds/all.rss.xml" blog)
          ("https://drewdevault.com/blog/index.xml" blog)
          ("https://lobste.rs/t/emacs.rss" emacs aggregator)
          ("https://reddit.com/r/emacs.rss" emacs aggregator)))

  (setq-default elfeed-search-filter "@3-months-ago +unread -aggregator")

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "1 month ago"
                                :remove 'unread))

  (leader-def
    "SPC n" 'elfeed)

  ;; Update by default each time the server starts
  (elfeed-update))

;; elfeed-web allows a basic web interface in http://localhost:8080/elfeed/
(use-package elfeed-web
  :ensure t
  :after elfeed
  :commands elfeed-web-start)


(provide 'init-rss)
;;; init-rss.el ends here
