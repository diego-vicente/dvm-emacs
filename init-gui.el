;;; init-gui.el --- Diego Vicente's personal Emacs GUI configuration
;; Maintainer: mail@diego.codes

;;; Commentary:
;; This file configures a set of usability and aesthetic knobs, as well as
;; installing all packages that are somehow required to achieve the look and
;; feel desired by the editor.

;;; Code:

;; Remove all clutter
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'nobreak-space nil :underline nil)
(setq-default cursor-type 'bar)

;; Set the custom font for all new frames created
(defvar default-font "Iosevka 12")

(defun set-custom-font (frame)
  "Set the default font in a new given FRAME."
  (interactive)
  (set-face-attribute 'default frame :font default-font)
  (set-face-attribute 'variable-pitch frame :font default-font)
  (set-face-attribute 'fixed-pitch frame :font default-font)
  (set-face-attribute 'tooltip frame :font default-font))

(add-to-list 'after-make-frame-functions 'set-custom-font t)

;; Define the color scheme
(use-package doom-themes
  :ensure t
  :demand t
  :config
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :init
  (load-theme 'doom-nord t)
  (doom-themes-org-config)
  :bind (("C-c c d" . (lambda () (interactive)
                        (load-theme 'doom-nord t)))
         ("C-c c l" . (lambda () (interactive)
                        (load-theme 'doom-solarized-light t)))))

;; Use the package to mark the lines changed respect to git
(use-package git-gutter
  :ensure git-gutter-fringe
  :hook ((prog-mode . git-gutter-mode)
         (org-mode . git-gutter-mode)))

;;; mode-line configuration

(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter "[+]")
  (minions-mode))

(use-package moody
  :ensure t
  :preface
  (defun dvm/set-moody-face (frame)
    (let ((line (face-attribute 'mode-line :underline frame)))
      (set-face-attribute 'mode-line          frame :overline   line)
      (set-face-attribute 'mode-line-inactive frame :overline   line)
      (set-face-attribute 'mode-line-inactive frame :underline  line)
      (set-face-attribute 'mode-line          frame :box        nil)
      (set-face-attribute 'mode-line-inactive frame :box        nil)))

  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq-default x-underline-at-descent-line t
                column-number-mode t)
  (add-to-list 'after-make-frame-functions 'dvm/set-moody-face t))

;; Enable the use of icons
(use-package font-lock+
  :demand t)

(use-package all-the-icons
  :after font-lock+
  :ensure t)


;;; init-gui.el ends here
