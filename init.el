;; Loads everything from the real configuration
(if (file-directory-p "~/nixos-setup")
	(setq configuration-dir "~/nixos-setup/my-emacs/")
  (setq configuration-dir "~/my-emacs/"))

(org-babel-load-file (concat configuration-dir "README.org"))
