;;; init.el --- Diego Vicente's personal Emacs configuration starter
;; Maintainer: mail@diego.codes

;;; Commentary:
;; This file is the entry point for my personal configuration, and serves as
;; basic boilerplate code for the rest of the configuration.

;;; Code:

;; Define the global variables to be used later
(defvar configuration-dir "~/dvm-emacs/")

;; Ensure that package.el is loaded and ready
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Add the utils folder to host all other packages
(add-to-list 'load-path (concat configuration-dir "utils"))


(defun dvm/ensure-package (package &optional version)
  "Ensure that a PACKAGE is installed.
Optionally, a minimum VERSION can be provided to check agains the
installed one."
  (if (package-installed-p package version)
      t
    (progn
      (unless (assoc package package-archive-contents)
        (package-refresh-contents))
      (package-install package))))

;; Ensure that use-package is installed and ready to use
(dvm/ensure-package 'use-package)
(require 'use-package)


(defun dvm/load-config-file (file)
  "Load a FILE present in the Emacs configuration."
  (let ((path (expand-file-name file configuration-dir)))
    (load-file path)))

;; Load the rest of the files in the configuration repository
(dvm/load-config-file "./init-usage.el")
(dvm/load-config-file "./init-gui.el")
(dvm/load-config-file "./init-tools.el")
(dvm/load-config-file "./init-org.el")
(dvm/load-config-file "./init-python.el")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-plus-contrib zeal-at-point yaml-mode ws-butler which-key vterm use-package undo-tree sphinx-doc solarized-theme smartparens sane-term rmsbolt request-deferred real-auto-save rainbow-delimiters racer python-docstring pyenv-mode org-ref org-pdfview org-mime org-brain org-autolist nord-theme nim-mode mu4e-alert moody minions magit-todos magit-popup magit-lfs lsp-ui lsp-intellij kotlin-mode jinja2-mode iy-go-to-char ivy-rich intero iedit idris-mode gruvbox-theme graphviz-dot-mode graphql go-playground go-eldoc git-gutter-fringe ghub flycheck-rust find-file-in-project expand-region exec-path-from-shell ess elpy elfeed-org ein eglot doom-themes dockerfile-mode docker dired-sidebar csv-mode counsel-tramp counsel-projectile counsel-bbdb conda company-lsp company-go company-auctex cider cheat-sh buffer-move bbdb avy all-the-icons-dired))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
