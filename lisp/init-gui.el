
;;; This file sets up different GUI packages to obtain the desired
;;; look-and-feel for the editor. It provides not only aesthetic
;;; packages but also help tooltips and other utilities for better
;;; usage.

;;; Code:

;; Load my own custom theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "lisp" dvm/user-configuration-directory))

;; TODO: define it as a trondheim-theme dependence?
(use-package autothemer
  :ensure t)

(load-theme 'trondheim t)

;; Enable relative line numbers for regular buffers
(defun dvm/enable-relative-line-numbers ()
  (interactive)
  (setq-local display-line-numbers 'relative))

(add-hook 'prog-mode-hook #'dvm/enable-relative-line-numbers)
(add-hook 'text-mode-hook #'dvm/enable-relative-line-numbers)

;; Set the mode-line using moody
(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t
        line-number-mode t
        column-number-mode t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; Enable the minor-mode menu using minions
(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter "[+]")
  (minions-mode t))

;; rainbow-delimiters uses colors to match pairs of delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; hl-todo colours different keyword in comments
(use-package hl-todo
  :ensure t
  :init
  (setq hl-todo-keyword-faces
        `(("FIXME"  . ,trondheim-nord11)
          ("GOTCHA" . ,trondheim-nord12)
          ("TODO"   . ,trondheim-nord13)
          ("DEBUG"  . ,trondheim-nord15)))
  :config
  (leader-def
    ;; move to the next to-do
    ". t" 'hl-todo-next)
  (global-hl-todo-mode))


;; which-key provides a pop-up menu with all available key bindings
(use-package which-key
  :ensure t
  :config
  ;; FIXME: the echo-area obstructs the last row sometimes.
  (setq which-key-pop-type 'side-window
        which-key-side-window-side-location 'bottom
        which-key-min-display-lines 6
        which-key-add-column-padding 2
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil)
  (which-key-mode t)
  :diminish which-key-mode)

;; popper defines a set of rules to treat different buffers as popups.
(use-package popper
  :ensure t
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          help-mode
          compilation-mode))
  :config
  (popper-mode +1)

  (defmacro dvm/run-in-popup (&rest body)
    "Run `BODY' inside a popup window."
    `(save-excursion
       (split-window-below)
       ,@body
       (popper-toggle-type)))

  (leader-def
    "k" '(:ignore t :which-key "popup")
    "k k" 'popper-toggle-latest
    "k l" 'popper-cycle
    "k ." 'popper-toggle-type))


(provide 'init-gui)
;;; init-gui.el ends here
