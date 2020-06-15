;;; init-python.el --- Diego Vicente's personal Emacs Python configuration
;; Maintainer: mail@diego.codes

;;; Commentary:
;; This file configures all needed settings to correctly work with Python in
;; Emacs.

;;; Code:

(add-to-list 'exec-path "/home/dvicente/utils/pyenv/shims")


(use-package pyvenv
  :ensure t
  :after pyenv-mode
  :preface
  (setq pyenv-home "/home/dvicente/utils/pyenv")

  (defun dvm/get-current-poetry-env ()
    "If available, return the path to the current poetry env."
    (let ((output (shell-command-to-string "poetry env info --path"))
          (error-regexp (rx anything "RuntimeError" anything)))
      (if (not (string-match-p error-regexp  output))
          output)))

  (defun dvm/activate-poetry-current ()
    "If the current project has an environment, activate it."
    (interactive)
    (if-let ((current (dvm/get-current-poetry-env)))
        (pyvenv-activate current)))

  (defun dvm/set-python-version (version)
    "Activate a certain pyenv VERSION for Emacs."
    (progn
      (pyvenv-activate (concat pyenv-home "/versions/" version))
      (message (concat "Setting python version: " version))))

  (defun dvm/pyenv-activate ()
    "Activate a certain pyenv version for Emacs."
    (interactive)
    (let ((version (read-from-minibuffer "Python version: ")))
      (dvm/set-python-version version)))

  (defun dvm/pyenv-global ()
    "Activate a certain pyenv version for Emacs."
    (interactive)
    (let ((version (shell-command-to-string "pyenv version-name")))
      (dvm/set-python-version version)))

  :hook (python-mode . dvm/activate-poetry-current))


(use-package sphinx-doc
  :ensure t
  :hook (python-mode . sphinx-doc-mode))


(use-package python-docstring
  :ensure t
  :hook (python-mode . python-docstring-mode))


(use-package python-black
  :demand t
  :hook (python-mode . python-black-on-save-mode))

;;; init-python.el ends here
