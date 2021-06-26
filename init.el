;;; init.el --- Load the full configuration
;;; Commentary:

;;; This file bootstraps the whole configuration, making some required
;;; checks and performing all the necessary imports.

;;; Code:

;; Store backtraces in case errors occur -- disabled by default
;; (setq debug-on-error t)

;; Save the configuration directory for future use, in case the git
;; repository is cloned elsewhere please edit this variable
(defvar dvm/user-configuration-directory "~/etc/dvm-emacs")

;; Define a custom file to not pollute init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Include the ./lisp directory in the path, since all the files are
;; there
(add-to-list 'load-path
             (expand-file-name "lisp" dvm/user-configuration-directory))

;; Require the home-brewed functions and utilities
(require 'dvm-functions)

;; Initialize the basic environment and packages
(require 'init-packages)
(require 'init-defaults)
(require 'init-evil)
(require 'init-gui)
(require 'init-completion)

;; Initialize the must-have tools
(require 'init-vc)
(require 'init-projectile)
(require 'init-lsp)
(require 'init-nix)

;; Initialize language-specific setups
(require 'init-python)

;; Initialize some other modes
(require 'init-irc)


(provide 'init)
;;; init.el ends here
