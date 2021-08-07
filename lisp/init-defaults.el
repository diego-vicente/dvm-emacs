;;; init-defaults.el --- Set all the Emacs default values
;;; Commentary:

;;; This file serves as a common place to set all the Emacs default
;;; values to more sensible values for the use intended. Since Emacs
;;; is a fairly piece of software, most of the defaults may seem alien
;;; at first. Some others may simply be intended for a different use
;;; case than the one for this configuration.

;;; Code:

;; Disable the different bars provided by default
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Always disable the scroll bar (for all frames)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

;; Make all prompts be simple y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Tune the scrolling to my very own personal preference
(setq scroll-step 1
      scroll-conservatively 101
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; Always remove trailing spaces before saving a file
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Indent with spaces by default (can be changed per language later)
(setq-default indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4)

;; delete-selection-mode enables overwriting selected text, which is
;; exactly what I would expect it to do
(delete-selection-mode t)

;; Set the default fill-column and some mode-specific values.
(setq-default fill-column 79)
(dvm/setq-hook prog-mode-hook comment-auto-fill-only-comments t)
(dvm/setq-hook text-mode-hook fill-column 72)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Auto-save and backup defaults -- Perform them often, but put the
;; files in dedicated directories instead of polluting the current
;; workspace
(setq auto-save-interval 20
      backup-by-copying t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      version-control t
      vc-make-backup-files t)

(defvar dvm/backup-directory
  (expand-file-name "backups" user-emacs-directory))

(setq backup-directory-alist
      `((".*" . ,(expand-file-name "per-save" dvm/backup-directory))))

(defvar dvm/auto-save-directory
  (expand-file-name "auto-save" user-emacs-directory))

(setq auto-save-file-name-transforms
      `((".*" ,dvm/auto-save-directory t)))

;; Define graphic pop-up frames, since I usually use a tiling wm
(setq-default pop-up-frames 'graphic-only)

;; Override the usual way of closing windows
(advice-add 'quit-window :around #'dvm/quit-window-advice)

;; Set some general purpose variables
(defvar dvm/password-directory "~/etc/dotfiles/passwords/"
  "Directory that stores the GPG encrypted passwords.")

;; The sole authentication file is bundled with the repository
(setq auth-sources
      `(,(expand-file-name "authinfo.gpg"
                           dvm/user-configuration-directory)))

(provide 'init-defaults)
;;; init-defaults.el ends here
