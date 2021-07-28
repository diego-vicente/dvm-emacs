;;; init-completion.el --- Set up auto-completion for Emacs
;;; Commentary:

;;; This file provides all the auto-completion tools expected for
;;; Emacs, which are mostly based on the ivy/counsel ecosystem. This
;;; file sets up all these configuration allowing for faster and more
;;; intuitive behavior in several places, as well as different search
;;; key-bindings that rely on them.

;;; Code:

(use-package ivy
  :ensure t
  :demand t
  :config
  (setq ivy-use-virtual-buffers t
    ivy-height 13
    ivy-count-format "%d/%d ")
  (ivy-mode))

(use-package swiper
  :ensure t
  :config
  (setq swiper-action-recenter t
    swiper-goto-start-of-match t)
  (leader-def
   "s" '(:ignore t :which-key "search")
   "s s" 'swiper-isearch
   "s S" 'swiper-isearch-thing-at-point
   "h F" 'counsel-describe-face))

;; Use company as an auto-completion backend
(use-package company
  :ensure t
  :config
  (setq company-tooltip-idle-delay 3)
  :hook (after-init . global-company-mode))

(use-package company-box
  :ensure t
  :config (setq company-box-doc-enable nil)
  :hook (company-mode . company-box-mode))

;; smartparens is a package to auto-close and manage parenthesis
(use-package smartparens-config
  :ensure smartparens
  :hook (prog-mode . smartparens-mode))

;; yasnippet provides a way to insert snippets into buffers
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs `(,(expand-file-name "snippets" dvm/user-configuration-directory)))
  ;; Enable it in all modes, since the snippets are mode-specific
  (yas-global-mode 1)
  (leader-def
    "c" '(:ignore t :which-key "complete")
    "c y" 'yas-insert-snippet))

(provide 'init-completion)
;;; init-completion.el ends here
