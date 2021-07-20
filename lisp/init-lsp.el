;;; init-lsp.el --- Configure Language Server Protocol for Emacs
;;; Commentary:

;;; This file contains a general configuration of the Language Server
;;; Protocol client for Emacs,as well as different performance
;;; settings and personal preferences. This is just the
;;; language-agnostic boilerplate; different integrations with
;;; languages are done in their respective configurations.

;;; Code:

(use-package flycheck
  :ensure t
  :demand t)

(use-package lsp-mode
  :ensure t
  :demand t
  :init
  ;; TODO: consider lsp-auto-configure to nil if there is too much bloat
  (setq gc-cons-threshold (* 100 1024 1024)      ; 100 mb
        read-process-output-max (* 3 1024 1024)  ; 3 mb
        lsp-idle-delay 0.5
        lsp-keymap-prefix "C-c l"
        lsp-signature-mode nil
        lsp-signature-doc-lines 1
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable-diagnostics nil)

  :config
  ;; Configure the conditional format on save
  (defvar dvm/lsp-format-on-save-p nil
    "If t, formats the current buffer with LSP before saving.") 

  (defun dvm/conditional-format-with-lsp ()
    "Format with LSP if dvm/lsp-format-on-save-p is t."
    (when dvm/lsp-format-on-save-p
      (lsp-format-buffer)))

  (defun dvm/format-with-lsp-on-save ()
    "Enable format on save with the LSP utilities."
    (interactive)
    (setq-local dvm/lsp-format-on-save-p t)
    (add-hook 'before-save-hook #'dvm/conditional-format-with-lsp))

  ;; Disable all other checkers but LSP
  (defun dvm/disable-checkers-for-lsp ()
    "Disable all Flycheck checkers except LSP."
    (interactive)
    (setq-local flycheck-enabled-checkers '(lsp)))

  ;; Edit the appearance of the doc popup
  (defun dvm/lsp-ui-doc-frame-set-face (frame _w)
    "Apply all face customization to the created tooltip frame."
    (set-face-attribute 'default frame :background trondheim-nord01))

  (add-hook 'lsp-ui-doc-frame-hook #'dvm/lsp-ui-doc-frame-set-face)

  ;; Configure the general LSP leader keys
  (leader-def
    :keymaps 'lsp-mode-map
    "l" '(:ignore t :which-key "lsp")
    "l e" 'flycheck-list-errors)
  ;; Per-mode hooks are defined in each language's own config
  :hook (lsp-mode . lsp-enable-which-key-integration))

;; lsp-ui provides different elements to consume the LSP info
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-delay 0.5
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-ignore-duplicate nil
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-ui-doc-delay 10
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-alignment 'frame
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t
        lsp-ui-imenu-colors (list trondheim-nord07
                                  trondheim-nord09
                                  trondheim-nord08
                                  trondheim-nord10))

  (leader-def
    :keymaps 'lsp-mode-map
    "l d" 'lsp-ui-peek-find-definitions
    "l r" 'lsp-ui-peek-find-references
    "l ?" 'lsp-ui-doc-show
    "l /" 'lsp-ui-doc-hide
    "l i" 'lsp-ui-imenu))

;; LSP and ivy integration
(use-package lsp-ivy
  :ensure t
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)

;; Enable the debugger mode (for compatible languages)
(use-package dap-mode
  :ensure t
  :after lsp-mode)


(provide 'init-lsp)
;;; init-lsp.el ends here
