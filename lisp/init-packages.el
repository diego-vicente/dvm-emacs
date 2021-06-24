;;; init-packages.el --- Set all the package management utilities
;;; Commentary:

;;; This file enable all the package management repositories as well
;;; as some useful tools as use-package, to be extensively used in
;;; this configuration.

;;; Code:

(require 'package)

;; Add MELPA to the archives
(add-to-list 'package-archives
             '( "melpa" . "https://melpa.org/packages/") t)

;; Bootstrap use-package and enable it
(unless (package-installed-p 'use-package)
  (message "Installing use-package for the first time...")
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


;; Define different byte-compilation functions for faster startup times.
(defun dvm/recompile-configuration (&optional force)
  "Compile to byte code all changes in the Emacs config."
  (interactive)
  (byte-recompile-directory dvm/user-configuration-directory force)
  (byte-recompile-directory user-emacs-directory force))

(defun dvm/delete-elc-for-current-buffer ()
  "Delete the associated compiled file when saving a new version."
  (interactive)
  (let ((compiled-filename (concat (buffer-file-name) "c")))
    (when (file-exists-p compiled-filename)
      (delete-file compiled-filename))))

(defun dvm/invalidate-compiled-on-save ()
  "Delete the compiled code each time a new version is saved."
  (add-hook 'after-save-hook
            #'dvm/delete-elc-for-current-buffer
            nil t))

;; Remove byte compiled files when saving an elips buffer.
(add-hook 'emacs-lisp-mode-hook 'dvm/invalidate-compiled-on-save)

;; Install straight.el - always used using :straight in use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(provide 'init-packages)
;;; init-packages.el ends here
