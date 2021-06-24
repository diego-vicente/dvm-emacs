;;; init-nix.el --- Configure all Nix/NixOS tools for Emacs
;;; Commentary:

;;; This file contains all tools that are normally used for a useful
;;; setup using Nix or NixOS, including Nix editing tools and
;;; integration with common utilities like direnv.

;;; Code:

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package direnv
  :ensure t
  :demand t
  :config
  (setq direnv-always-show-summary nil)
  ;; Switch direnv as soon as the project changes
  (add-to-list 'direnv--hooks
               'projectile-after-switch-project-hook)
  (direnv-mode))


(provide 'init-nix)
;;; init-nix.el ends here
