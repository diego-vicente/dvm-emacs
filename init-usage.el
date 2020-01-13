;;; init-usage.el --- Diego Vicente's personal Emacs usage settings
;; Maintainer: mail@diego.codes

;;; Commentary:
;; This file contains a set of tunings that allow the editor to have a specific
;; set of defaults at startup, all of them according to the user preferences.

;;; Code:

;;; Basic cofiguration settings:

;; Configure startup buffer as an org-mode buffer
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Change all the yes-or-no prompt to single key input
(fset 'yes-or-no-p 'y-or-n-p)

;; Allow to kill buffers with an associated process as regular buffers
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Try to make scroll as controlled as possible
(setq scroll-step            1
      scroll-conservatively  10000
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

;; Other minor configurations
(setq-default ring-bell-function 'ignore
              save-abbrevs 'silent
              indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4
              backup-directory-alist '(("." . "~/.emacs.d/backup"))
              backup-by-copying t
              version-control t
              delete-old-versions t
              kept-new-versions 20
              kept-old-versions 5
              sentence-end-double-space nil)

(unbind-key "C-z")

(delete-selection-mode t)

;; Import global variables and PATH to the exec-path
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "DESKTOP_SESSION"))
  (exec-path-from-shell-initialize))


;;; Miscellaneous custom elips functions:

;; Make auto-fill work on comments by default
(defun dvm/comment-auto-fill ()
  "Make the current buffer have its comments auto-filled."
  (interactive)
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(add-hook 'prog-mode-hook 'dvm/comment-auto-fill)

;; Shortcut to open the configuration file
(defun dvm/open-emacs-config ()
  "Open the startup configuration file."
  (interactive)
  (find-file (concat configuration-dir "init.el")))

(global-set-key (kbd "C-c c o") 'dvm/open-emacs-config)

;; Improve the behavior of C-a
(defun dvm/beginning-of-line-dwim ()
  "Move to the beginning of the intentation.
If already in the beginning of the indentation, go to the
beginning of the line. This function allows to easily go to the
start of the code in one keystroke or to the beginning of the
line in two."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(global-set-key (kbd "C-a") 'dvm/beginning-of-line-dwim)

;; Include a key to copy but not delete a line.
(defun dvm/dont-kill-line()
  "Copy fromm the point to the end of the line without deleting it."
  (interactive)
  (kill-line)
  (yank))

(global-set-key (kbd "M-k") 'dvm/dont-kill-line)


;; Set up Emacs to run in i3
;; Fix quit-window definitions to get rid of buffers
(defun dvm/quit-window-dwim (&optional args)
  "`delete-frame' if single window, else `quit-window' and ARGS."
  (interactive)
  (if (one-window-p)
      (delete-frame)
    (quit-window args)))

(defun dvm/set-up-i3 ()
  "Set up Emacs to run inside i3 with no frills."
  (interactive)
  (setq-default pop-up-frames 'graphic-only
                magit-bury-buffer-function 'dvm/quit-window-dwim
                magit-commit-show-diff nil)
  (substitute-key-definition 'quit-window 'dvm/quit-window-dwim
                             global-map)
  (substitute-key-definition 'quit-window 'dvm/quit-window-dwim
                             help-mode-map)
  (substitute-key-definition 'quit-window 'dvm/quit-window-dwim
                             compilation-mode-map)
  (substitute-key-definition 'quit-window 'dvm/quit-window-dwim
                             Buffer-menu-mode-map)
  (substitute-key-definition 'quit-window 'dvm/quit-window-dwim
                             org-brain-visualize-mode-map)
  (message "Configuration for i3 applied"))

(add-hook 'after-init-hook 'dvm/set-up-i3)

;;; usage.el ends here
