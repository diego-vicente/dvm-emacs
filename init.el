;; Initialize package
(package-initialize)

;; Loads everything from the real configuration
(if (file-directory-p "~/nixos-setup")
	(setq configuration-dir "~/nixos-setup/my-emacs/")
  (setq configuration-dir "~/my-emacs/"))

;; Autoload use-package
(eval-when-compile
  (require 'use-package))

;; Load the literate configuration
(org-babel-load-file (concat configuration-dir "README.org"))
