;;; init-org.el --- Diego Vicente's personal org-mode configuration
;; Maintainer: mail@diego.codes

;;; Commentary:
;; This file defines all custom settings for my org-mode usage, including some
;; other packages that make use of org-mode.

;;; Code:
(defvar org-dir "~/Dropbox/org")

(use-package org
  :pin org
  :ensure org-plus-contrib
  :preface (defvar dvm/org-keywords
             '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "DROP(x!)")))

  (defun dvm/org-confirm-evaluate (lang body)
    "Enable babel evaluation for LANG in BODY."
    (not (member lang '("emacs-lisp" "python" "sh" "dot"))))

  (defun dvm/fix-inline-images ()
    "Properly display images in org-mode."
    (when org-inline-image-overlays
      (org-redisplay-inline-images)))

  (defun dvm/org-format-bold (&optional arg)
    "Surround the selected text with asterisks (bold)"
    (interactive "P")
    (insert-pair arg ?\* ?\*))

  (defun dvm/org-format-italics (&optional arg)
    "Surround the selcted text with forward slashes (italics)"
    (interactive "P")
    (insert-pair arg ?\/ ?\/))

  (defun dvm/org-format-tt (&optional arg)
    "Surround the selcted text with virgules (monotype)"
    (interactive "P")
    (insert-pair arg ?\= ?\=))

  :init (setq-default fill-column 79
                      org-todo-keywords dvm/org-keywords
                      org-log-into-drawer t
                      org-src-preserve-indentation t
                      org-confirm-babel-evaluate 'dvm/org-confirm-evaluate
                      org-src-tab-acts-natively t
                      org-image-actual-width 620
                      org-startup-folded 'content
                      org-src-fontify-natively t
                      org-hide-leading-stars t
                      org-ellipsis " ⤵"
                      org-pretty-entities t
                      org-latex-create-formula-image-program 'dvisvgm)

  :config (org-babel-do-load-languages
           'org-babel-load-languages
           '((shell . t) (python . t)))

  :bind (:map org-mode-map
              ("C-c f b" . dvm/org-format-bold)
              ("C-c f i" . dvm/org-format-italics)
              ("C-c f m" . dvm/org-format-tt)
              ("C-c f *" . dvm/org-format-bold)
              ("C-c f /" . dvm/org-format-italics)
              ("C-c f =" . dvm/org-format-tt))

  :hook ((org-mode . auto-fill-mode)
         (org-babel-after-execute . dvm/fix-inline-images)))


(use-package org-autolist
  :ensure t
  :after org
  :hook (org-mode . org-autolist-mode))


(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-notes (concat org-dir "/bibliography/notes.org")
        org-ref-default-bibliography '((concat org-dir "/bibliography/main.bib"))
        org-ref-pdf-directory (concat org-dir "/bibliography/pdfs")
        org-ref-insert-cite-function 'org-ref-helm-insert-cite-link
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


(use-package org-brain
  :ensure t
  :demand t
  :init
  (setq org-brain-path (concat org-dir "/brain/"))
  :config
  (setq org-id-track-globally t
        org-id-locations-file "~/.emacs.d/.org-id-locations"
        org-brain-visualize-default-choices 'all
        org-brain-title-max-length 30)
  (org-brain-update-id-locations)
  :bind (("C-c b" . org-brain-visualize)))

(use-package ox-publish
  :init
  (setq my-blog-header-file "~/Projects/blog/org/partials/header.html"
        my-blog-footer-file "~/Projects/blog/org/partials/footer.html"
        org-html-validation-link nil)

  ;; Load partials on memory
  (defun dvm/blog-header (arg)
    (with-temp-buffer
      (insert-file-contents my-blog-header-file)
      (buffer-string)))

  (defun dvm/blog-footer (arg)
    (with-temp-buffer
      (insert-file-contents my-blog-footer-file)
      (buffer-string)))

  (defun dvm/filter-local-links (link backend info)
    "Filter that converts all the /index.html links to /"
    (if (org-export-derived-backend-p backend 'html)
        (replace-regexp-in-string "/index.html" "/" link)))

  :config
  (setq org-publish-project-alist
        '(;; Publish the posts
          ("blog-notes"
           :base-directory "~/Projects/blog/org"
           :base-extension "org"
           :publishing-directory "~/Projects/blog/public"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :section-numbers nil
           :html-head nil
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           :html-preamble dvm/blog-header
           :html-postamble dvm/blog-footer)

          ;; For static files that should remain untouched
          ("blog-static"
           :base-directory "~/Projects/blog/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|eot\\|svg\\|woff\\|woff2\\|ttf"
           :publishing-directory "~/Projects/blog/public"
           :recursive t
           :publishing-function org-publish-attachment)

          ;; Combine the two previous components in a single one
          ("blog" :components ("blog-notes" "blog-static"))))

  (add-to-list 'org-export-filter-link-functions 'dvm/filter-local-links))

;;; init-org.el ends here
