;;; init-markup.el --- Configure markup languages for Emacs
;;; Commentary:

;;; This file includes several configurations for different markup languages
;;; that are required in a day-to-day basis. These are probably most of the
;;; popular languages that I may encounter.

;;; Code:

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(use-package toml-mode
  :ensure t
  :commands toml-mode)


(provide 'init-markup)
;;; init-markup.el ends here.
