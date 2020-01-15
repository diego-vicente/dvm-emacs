;;; init-tools.el --- Diego Vicente's personal Emacs package tools
;; Maintainer: mail@diego.codes

;;; Commentary:
;; This file contains a set of mode-agnostic tools that I enjoy using in the
;; editor. The declarations ensure that the package is installed and configure
;; everything necessary.

;;; Code:

(use-package compile
  :demand t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package dired
  :config (setq dired-dwim-target t)
  :hook (dired-mode . dired-hide-details-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-count-format "%d/%d ")
  :bind (("C-s" . swiper)
         ("C-c h f" . counsel-describe-function)
         ("C-c h v" . counsel-describe-variable)
         ("M-i" . counsel-imenu)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-j" . ivy-done)))

(use-package ws-butler
  :ensure t
  :config (ws-butler-global-mode 1))


(use-package magit
  :ensure t
  :config
  (setq magit-section-initial-visibility-alist '((unpushed . show)))
  (git-commit-turn-on-auto-fill)
  (add-hook 'git-commit-mode-hook (lambda () (setq-local fill-column 72)))
  :bind (("C-x g" . magit-status)))

(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode))


(use-package projectile
  :ensure t
  :preface
  (defun dvm/colorize-compilation-buffer ()
    "Colorize a (compilation) buffer using ansi-colors."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :config
  (projectile-global-mode 1)
  (define-key projectile-mode-map (kbd "C-C p") 'projectile-command-map)
  (setq projectile-use-git-grep t
        ;; Fix for compilation command and `generic` projects
        projectile-project-compilation-cmd ""
        projectile-project-run-cmd ""
        projectile-project-test-cmd "")

  (ignore-errors
    (require 'ansi-color)
    (add-hook 'compilation-filter-hook 'dvm/colorize-compilation-buffer)))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config (counsel-projectile-mode t))


(use-package company
  :ensure t
  :demand t
  :config (setq company-tooltip-align-annotations t))


(use-package lsp-mode
  :ensure t
  :config (setq-default lsp-enable-symbol-highlighting nil)
  :hook (python-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :config
  (defun dvm/lsp-ui-doc-dwim ()
    "Toggle lsp-ui-doc-mode tooltips on demand."
    (interactive)
    (if lsp-ui-doc-mode
        (lsp-ui-doc-mode -1)
      (lsp-ui-doc-mode)))

  (defun dvm/lsp-ui-doc-tooltip-appearance (frame _w)
    "Apply all face customization to the created tooltip frame."
    (set-face-attribute 'default frame :font "Iosevka SS10"))

  (setq-default lsp-ui-doc-delay 0
                lsp-ui-doc-enable nil
                lsp-ui-sideline-enable t)

  :commands lsp-ui-mode
  :bind (:map lsp-mode-map ("C-c ?" . dvm/lsp-ui-doc-dwim))
  :hook (lsp-ui-doc-frame . dvm/lsp-ui-doc-tooltip-appearance))

(use-package company-lsp
  :ensure t
  :commands company-lsp)


(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package vterm
    :ensure t
    :bind (("C-c t" . vterm-other-window)))


(use-package iedit
  :ensure t
  :bind (("C-c i" . iedit-mode)))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package flycheck
  :ensure t
  :config
  (set-face-underline 'flycheck-error '(:color "#dc322f" :style line))
  (set-face-underline 'flycheck-warning '(:color "#e5aa00" :style line))
  (set-face-underline 'flycheck-info '(:color "#268bd2" :style line))
  :hook (prog-mode . flycheck-mode))

(use-package flyspell
  :ensure t
  :preface
  (defun dvm/change-dictionary-spanish ()
    "Change the spell checker to Spanish."
    (interactive)
    (ispell-change-dictionary "espanol"))

  (defun dvm/change-dictionary-english ()
    "Change the spell checker to English."
    (interactive)
    (ispell-change-dictionary "english"))

  :config
  (set-face-underline  'flyspell-incorrect '(:color "#dc322f" :style line))
  (set-face-underline  'flyspell-duplicate '(:color "#e5aa00" :style line))

  (setq ispell-program-name "aspell"
        ispell-dictionary "english")
  :hook (text-mode . flyspell-mode))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; init-tools.el ends here
