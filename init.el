;;; init.el --- Diego Vicente's personal Emacs configuration starter
;; Maintainer: mail@diego.codes

;;; Commentary:
;; This file is the entry point for my personal configuration, and serves as
;; basic boilerplate code for the rest of the configuration.

;;; Code:

;; Define the global variables to be used later
(defvar configuration-dir "~/utils/dvm-emacs/")

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

;; Finally load (or create) the custom file
(setq custom-file "~/.emacs.d/custom.el")

(condition-case _
    (load custom-file)
  (file-missing (write-region "" nil custom-file)))


;;; init.el ends here
