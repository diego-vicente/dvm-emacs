;;; init-org.el --- Configure org-mode for Emacs
;;; Commentary:

;;; This file contains all the configuration for org-mode [1], the
;;; almighty organization mode and file format.

;;; [1]: https://orgmode.org/

;;; Code:

;; Require and configure the main org-mode package
(use-package org
  :ensure t
  :after evil
  :config
  (setq org-directory (expand-file-name "~/docs/org")
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  ;; Define evil operators for org-mode
  (evil-define-operator dvm/format-org-bold (beg end type)
    "Surround the text object in the bold markup."
    (interactive "<R>")
    (evil-surround-region beg end t ?* nil))

  (evil-define-operator dvm/format-org-italics (beg end type)
    "Surround the text object in the italics markup."
    (interactive "<R>")
    (evil-surround-region beg end t ?/ nil))

  (evil-define-operator dvm/format-org-monospace (beg end type)
    "Surround the text object in the monospace markup."
    (interactive "<R>")
    (evil-surround-region beg end t ?= nil))

  (leader-def
    :keymaps 'org-mode-map
    "o" '(:ignore t :which-key "org")
    "o o" 'org-open-at-point
    "o b" 'org-insert-structure-template
    "o f" '(:ignore t :which-key "format")
    "o f b" 'dvm/org-format-bold
    "o f *" 'dvm/org-format-bold
    "o f i" 'dvm/org-format-italics
    "o f /" 'dvm/org-format-italics
    "o f m" 'dvm/org-format-monospace
    "o f =" 'dvm/org-format-monospace))

;; enable active source blocks using org-babel
(use-package ob-shell
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))

;; org-roam provides a way to generate a Zettelkasten
(use-package org-roam
  :ensure t
  :after org
  :init (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory (expand-file-name "zettelkasten" org-directory))

  (leader-def
    "z" '(:ignore t :which-key "org-roam")
    "z l" 'org-roam-buffer-toggle
    "z f" 'org-roam-node-find
    "z g" 'org-roam-graph
    "z i" 'org-roam-node-insert
    "z c" 'org-roam-capture)

  (org-roam-setup))


(provide 'init-org)
;;; init-org.el ends here.
