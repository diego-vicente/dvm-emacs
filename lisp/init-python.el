;;; init-python.el --- Set up the Python development environment
;;; Commentary:

;;; Code:

;; Let lsp-mode load first
(with-eval-after-load 'lsp-mode
  ;; Set the LSP to be enabled in Python automatically
  (add-hook 'python-mode-hook #'lsp)

  ;; Turn on format when saving
  (add-hook 'python-mode-hook #'dvm/format-with-lsp-on-save)

  ;; Make sure that LSP is the only checker for linting
  (add-hook 'python-mode-hook #'dvm/disable-checkers-for-lsp)

  ;; Configure the variables for python
  (setq lsp-pylsp-plugins-flake8-enabled t
        lsp-pylsp-plugins-yapf-enabled t
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-flake8-ignore ["E501"])

  ;;Set the LSP options with no proper variable yet
  (lsp-register-custom-settings
   '(("pylsp.plugins.mypy_ls.enabled" t t)
     ("pylsp.plugins.mypy_ls.live_mode" nil t)
     ("pylsp.plugins.pyls_isort.enabled" t t)
     ("pylsp.plugins.pyflakes.enabled" nil t))))


(provide 'init-python)
;;; init-python.el ends here
