;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Diego Vicente"
      user-mail-address "mail@diego.codes")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord
      doom-font "Iosevka 12")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Include auxiliar and other custom functions
(load (expand-file-name (concat doom-private-dir "/utils.el")))

;; Change the quit window behavior for some modes
;; (dvm/adapt-quit-window-for-mode global-map)
;; (dvm/adapt-quit-window-for-mode help-mode-map)
;; (dvm/adapt-quit-window-for-mode compilation-mode-map)
;; (dvm/adapt-quit-window-for-mode Buffer-menu-mode-map)
;; (dvm/adapt-quit-window-for-mode org-brain-visualize-mode-map)

;; Set some general defaults
(setq-default initial-major-mode 'org-mode
              ;; tab settings
              indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4
              ;; backup settings
              backup-by-copying t
              backup-directory-alist `("." . (concat user-emacs-directory "backups"))
              tramp-backup-directory-alist backup-directory-alist
              delete-old-versions t
              kept-new-versions 3
              kept-old-versions 2
              version-control t
              vc-cvs-stay-local nil
              ;; Desktop specific variables
              pop-up-frames 'graphic-only
              ;; Don't create new workspaces with new frames
              persp-init-new-frame-behaviour-override 'dvm/workspace-associate-frame-fn
              persp-emacsclient-init-frame-behaviour-override 'dvm/workspace-associate-frame-fn
              persp-interactive-init-frame-behaviour-override 'dvm/workspace-associate-frame-fn)

(remove-hook! 'delete-frame-functions '+workspaces-delete-associated-workspace-h)

;; Auto fill only comments in the programming modes
(setq-hook! prog-mode comment-auto-fill-only-comments t)
(add-hook! prog-mode auto-fill-mode)

;; Allow remembering risky varibales in .dirs-local.el
(advice-add 'risky-local-variable-p :override #'ignore)

;;; Major modes and other tools

(use-package! markdown-mode
  :commands (markdown-mode)
  :hook (markdown-mode . auto-fill-mode))

;; Rust config
(after! rustic
  (setq lsp-rust-server 'rust-analyzer
        rustic-lsp-server 'rust-analyzer))

;;; Python configuration

;; Set up the Python auto-formatter on save
(use-package! python-black
  :commands (python-black-on-save-mode)
  :hook (python-mode . python-black-on-save-mode))

;; org-mode configuration

(defvar org-dir "~/docs/Dropbox/org")

(use-package! org-brain
  :init (setq org-brain-path (concat org-dir "/brain/"))
  :config (setq org-id-track-globally t
                org-id-locations-file "~/.emacs.d/.org-id-locations"
                org-brain-visualize-default-choices 'all
                org-brain-title-max-length 30)
  (org-brain-update-id-locations)
  :commands (org-brain-visualize)
  :bind (("C-c b" . org-brain-visualize)))

(use-package! org-ref
  :commands (org-ref-insert-ref-link)
  :config
  (setq org-ref-bibliography-notes (concat org-dir "/bibliography/notes.org")
        org-ref-default-bibliography (concat org-dir "/bibliography/main.bib")
        org-ref-pdf-directory (concat org-dir "/bibliography/pdfs")
        ;; org-ref-insert-cite-function 'org-ref-helm-insert-cite-link
        org-latex-prefer-user-labels t
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5)
  (unless (file-exists-p org-ref-pdf-directory)
    (make-directory org-ref-pdf-directory t)))
