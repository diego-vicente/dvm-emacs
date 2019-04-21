;; Initialize package
(package-initialize)

;; Loads everything from the real configuration
(setq configuration-dir "~/dvm-emacs/")

;; Autoload use-package
(eval-when-compile
  (require 'use-package))

;; Load the literate configuration
(org-babel-load-file (concat configuration-dir "README.org"))
