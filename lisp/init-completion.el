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
  :hook (after-init . global-company-mode))

(use-package company-box
  :ensure t
  :config (setq company-box-doc-enable nil)
  :hook (company-mode . company-box-mode))


(provide 'init-completion)
;;; init-completion.el ends here
