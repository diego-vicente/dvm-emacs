;;; init-vc.el --- Configure version control in Emacs
;;; Commentary:

;;; This file includes all the necessary tools to use version control
;;; tools (for now, git) in Emacs, using the well-known Magit package.

;;; Code:

;; Magit is all is needed, to be honest
(use-package magit
  :ensure t
  :after general
  :config
  (setq magit-section-initial-visibility nil)
  ;; Define the bindings with the prefix `SPC g`
  (leader-def
   "g" '(:ignore nil :which-key "magit")
   "g g" 'magit-status
   "g c" 'magit-clone
   "g b" 'magit-blame))

;; Include all to-do tags in the magit status buffer
(use-package magit-todos
  :ensure t
  :after magit
  :config
  (setq magit-todos-branch-list nil)
  (magit-todos-mode))

;; Indicate changes from HEAD in the buffer gutter
(use-package git-gutter-fringe
  :demand t
  :ensure t
  :config
  ;; Make the fringe thinner
  (if (fboundp 'fringe-mode) (fringe-mode '4))
  ;; Places the git gutter outside the margins
  (setq fringes-outside-margins t)
  ;; Use the subtle fringe bitmaps from doom-emacs
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode)))


(provide 'init-vc)
;;; init-vc.el ends here
