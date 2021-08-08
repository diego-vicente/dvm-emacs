;;; init-projectile.el --- Configure project management in Emacs
;;; Commentary:

;;; This file include all the configuration needed for per-project
;;; shortcuts, based on the popular Projectile package.

;;; Code:

(use-package projectile
  :ensure t
  :after general popper
  :config
  ;; Ignore all projects in the nix store
  (defun dvm/ignore-project-p (project-name)
    (or
     (string-prefix-p "/nix" project-name)
     (string-prefix-p "/nix/store" project-name)
     (string-prefix-p "~/.emacs.d/" project-name)))

  (setq projectile-ignored-project-function #'dvm/ignore-project-p)

  (defun dvm/eshell-dwim ()
    "Turn on eshell as a small popup for a given context."
    (interactive)
    ;; FIXME: it does not work if the buffer exists already
    (dvm/run-in-popup
     (cond
      ((projectile-project-p) (projectile-run-eshell))
      (t (eshell)))))


  ;; Define the bindings with the prefix `SPC g`
  (leader-def
   "p" '(:ignore t :which-key "projectile")
   "p p" 'projectile-switch-project
   "p f" 'projectile-find-file
   "p c" 'projectile-compile-project
   "p t" 'projectile-test-project
   "p s" 'projectile-save-project-buffers
   "p v" 'projectile-run-vterm
   "p e" 'dvm/eshell-dwim)
  (projectile-mode))

;; counsel-projectile has great integration with ripgrep
(use-package counsel-projectile
  :ensure t
  :after projectile
  :commands counsel-rg counsel-projectile-rg
  :init
  (defun dvm/interactive-rg (directory)
    "Interactive wrapper around rgrep for a directory."
    (interactive "DSearch directory: ")
    (counsel-rg nil directory))

  (leader-def
    "s d" 'dvm/interactive-rg
    "s p" 'counsel-projectile-rg))


(provide 'init-projectile)
;;; init-vc.el ends here
