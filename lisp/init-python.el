;;; init-python.el --- Set up the Python development environment
;;; Commentary:

;;; This file configures a Python environment that leverages on Microsoft's
;;; Language Server Protocol [1] using python-lsp/python-lsp-server [2]
;;; implementation. This fork improves performance of the system and this
;;; configuration tries to achieve a relatively light configuration. Beware
;;; though, at the time of writing this, Python LSP is one of the most
;;; demanding implementations out there.

;;; [1]: https://microsoft.github.io/language-server-protocol/
;;; [2]: https://github.com/python-lsp/python-lsp-server

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
  (setq lsp-pylsp-configuration-sources ["flake8"]
        lsp-pylsp-plugins-flake8-enabled nil
        lsp-pylsp-plugins-yapf-enabled nil
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-pydocstyle-enabled nil
        lsp-pylsp-plugins-flake8-ignore
        [;; line too long
         "E501"
         ;; line break before binary operator
         "W503"])

  ;;Set the LSP options with no proper variable yet
  (lsp-register-custom-settings
   '(("pylsp.plugins.mypy_ls.enabled" nil t)
     ("pylsp.plugins.mypy_ls.live_mode" nil t)
     ("pylsp.plugins.pyls_isort.enabled" t t)
     ("pylsp.plugins.pyls_black.enabled" t t)
     ("pylsp.plugins.pyflakes.enabled" nil t))))

;; Configure the debugger
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))


(provide 'init-python)
;;; init-python.el ends here
