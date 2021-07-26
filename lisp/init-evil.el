;;; init-evil.el --- Configure vi(m) keybindings in Emacs
;;; Commentary:

;;; This file configures all utilities in Emacs to provide the vim
;;; keybindings and all the approximate equivalent in different menus
;;; and packages. It also includes different indicators for the state
;;; of the modal editing.

;;; Code:

;; Delegate integration to evil-collection
(setq evil-want-integration t
      evil-want-keybinding nil)

;; Define the leader key to be SPC
(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode))

;; Enable evil mode for modal editing
(use-package evil
  :ensure t
  :demand t
  :after evil-leader
  :config
  ;; Disable mode-line indicators
  (setq evil-mode-line-format nil)

  ;; Define all custom operators for evil-mode
  (evil-define-operator dvm/negate-word-at-point (beg end)
    "Change the current word at point with its opposite."
    :motion evil-inner-word
    (let* ((buffer-word (buffer-substring-no-properties beg end))
           (negated-word (dvm/negate-word buffer-word)))
      (if negated-word
          (progn
            (delete-region beg end)
            (insert negated-word))
        (message "No opposite word found in the list."))))

  (evil-define-operator dvm/transpose-chars (beg end)
    "Transpose the character"
    :motion evil-forward-char
    (goto-char end)
    (transpose-chars 1)
    (goto-char beg))

  (evil-mode t))

;; evil-collection fixes evil where is not supported by default
(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

;; evil-surround manages pairs of characters
(use-package evil-surround
  :ensure t
  :after evil
  :config
  ;; Override the default pairs
  (setq-default
   evil-surround-pairs-alist
   '(( ?\( . ("(" . ")"))
     ( ?\[ . ("[" . "]"))
     ( ?\{ . ("{" . "}"))
     ( ?\' . ("'" . "'"))
     ( ?\" . ("\"" . "\""))
     ;; org-mode markup syntax
     ( ?\* . ("*" . "*"))
     ( ?\/ . ("\/" . "\/"))
     ( ?\= . ("=" . "="))))

  (global-evil-surround-mode 1))

;; general is used to define SPC-prefixed keybindings
(use-package general
  :ensure t
  :after evil
  :config
  ;; Define the general leader key
  (general-create-definer leader-def
    :states 'normal
    :prefix "SPC"
    :keymaps 'override)

  ;; Define the evil normal-mode prefix
  (general-create-definer normal-z-def
    :states 'normal
    :prefix "z"
    :keymaps 'override)

  ;; Define the common key bindings
  (leader-def
   ;; buffer-related functions
   "b" '(:ignore t :which-key "buffer")
   "b b" 'switch-to-buffer
   "b k" 'kill-buffer
   ;; file-related functions
   "f" '(:ignore t :which-key "file")
   "f f" 'find-file
   "f d" 'dired
   ;; shortcut functions
   "." '(:ignore t :which-key "go-to")
   ". d" 'evil-goto-definition
   ;; help functions
   "h" '(:ignore t :which-key "help")
   "h f" 'describe-function
   "h v" 'describe-variable
   "h k" 'describe-key)

  ;; Define the normal state key bindings
  (normal-z-def
   ;; Define scroll functions
   "h" 'evil-scroll-column-left
   "j" 'evil-scroll-line-down
   "k" 'evil-scroll-line-up
   "l" 'evil-scroll-column-right
   "H" 'evil-scroll-left
   "J" 'evil-scroll-down
   "K" 'evil-scroll-up
   "L" 'evil-scroll-right
   ;; Other fast access functions
   ";" 'comment-dwim
   ;; Custom text manipulation functions
   "n" 'dvm/negate-word-at-point
   "t" 'dvm/transpose-chars))


(provide 'init-evil)
;;; init-evil.el ends here
