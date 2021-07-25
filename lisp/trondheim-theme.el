;;; trondheim.el --- dvicente's twist on Nord colors
;;; Commentary

;;; This file provides my very own theme, Trondheim, based on the very
;;; well known Nord color scheme [1] and the work done by Nicolas
;;; Rougier on editor configuration. It is an exercise of Lisp
;;; training and a way to have a bit more control on its settings,
;;; which could also be understood as bikeshedding.

;;; [1]: https://www.nordtheme.com/
;;; [2]: https://github.com/rougier/nano-emacs

;;; Code:

;; Using autothemer to abstract the boring parts
(require 'autothemer)

;; Export all the colors as variables
(define-advice autothemer-deftheme (:before (_ _ palette &rest _) defcolors)
(mapcar (lambda (e)
        (setf (symbol-value (car e))
                (cadr e)))
        (cdr palette)))

(autothemer-deftheme
 trondheim "Diego Vicente's personal version of Nord"

 ((((class color) (min-colors #xFFFFFF))) ; Only for graphical Emacs

  ;; Define the Nord color palette
  ;; Polar Night palette -- dark base colors
  (trondheim-nord00 "#2E3440")
  (trondheim-nord01 "#3B4252")
  (trondheim-nord02 "#434C5E")
  (trondheim-nord03 "#4C566A")
  ;; Snow Storm palette -- bright base colors
  (trondheim-nord04 "#D8DEE9")
  (trondheim-nord05 "#E5E9F0")
  (trondheim-nord06 "#ECEFF4")
  ;; Frost palette -- cold accent colors
  (trondheim-nord07 "#8FBCBB")
  (trondheim-nord08 "#88C0D0")
  (trondheim-nord09 "#81A1C1")
  (trondheim-nord10 "#5E81AC")
  ;; Aurora palette -- warmer support colors 
  (trondheim-nord11 "#BF616A")
  (trondheim-nord12 "#D08770")
  (trondheim-nord13 "#EBCB8B")
  (trondheim-nord14 "#A3BE8C")
  (trondheim-nord15 "#B48EAD")
  ;; Trondheim custom colors
  (trondheim-comment "#6f7787"))

 ;; Define the custom faces for the theme

 (;; Basic Emacs faces
  (default (:background trondheim-nord00 :foreground trondheim-nord05
            ;; TODO: maybe make the font setting conditional?
            :family "JetBrains Mono" :height 110))
  (bold    (:foreground trondheim-nord06 :weight 'bold))
  (fringe  (:background trondheim-nord00 :foreground trondheim-comment))
  (cursor  (:foreground trondheim-nord06))
  (link    (:foreground trondheim-nord09))
  (error   (:foreground trondheim-nord11 :weight 'bold))
  (warning (:foreground trondheim-nord13 :weight 'bold))
  (success (:foreground trondheim-nord14 :weight 'bold))
  (region  (:background trondheim-nord01))
  (shadow  (:foreground trondheim-comment))
  (highlight (:background trondheim-nord02))
  (minibuffer-prompt (:foreground trondheim-nord09))
  (tooltip (:background trondheim-nord04 :foreground trondheim-nord01))
  (header-line (:background trondheim-nord00
                :foreground trondheim-nord05
                :box (:line-width 7 :color trondheim-nord00)
                :overline nil
                :underline nil))

  ;; mode-line faces -- prepared for moody
  (mode-line  (:background trondheim-nord02
               :foreground trondheim-nord05
               :overline trondheim-comment
               :underline trondheim-comment
               :box nil))

  (mode-line-inactive (:background trondheim-nord01
                       :foreground trondheim-comment
                       :overline trondheim-nord02
                       :underline trondheim-nord02
                       :box nil
                       :weight 'regular))

  ;; Built-in syntax highlighting -- taken from Nord
  (font-lock-builtin-face       (:foreground trondheim-nord09))
  (font-lock-constant-face      (:foreground trondheim-nord09))
  (font-lock-function-name-face (:foreground trondheim-nord08))
  (font-lock-keyword-face       (:foreground trondheim-nord09))
  (font-lock-string-face        (:foreground trondheim-nord14))
  (font-lock-variable-name-face (:foreground trondheim-nord05))
  (font-lock-type-face          (:foreground trondheim-nord07))
  (font-lock-warning-face       (:foreground trondheim-nord13))
  (font-lock-comment-face       (:foreground trondheim-comment))
  (font-lock-doc-face           (:foreground trondheim-comment :slant 'italic))
  (font-lock-regex-grouping-backlash  (:foreground trondheim-nord13))
  (font-lock-regex-grouping-construct (:foreground trondheim-nord13))

  ;; ivy faces
  (ivy-current-match (:background trondheim-nord01))
  (ivy-minibuffer-match-face-1 (:inherit 'ivy-current-match))
  (ivy-minibuffer-match-face-2 (:background trondheim-nord13 :foreground trondheim-nord01))
  (ivy-minibuffer-match-face-3 (:background trondheim-nord14 :foreground trondheim-nord01))
  (ivy-minibuffer-match-face-4 (:background trondheim-nord15 :foreground trondheim-nord01))
  (ivy-remote (:foreground trondheim-nord07))
  (ivy-postframe (:background trondheim-nord01))
  (ivy-postframe-border (:background trondheim-nord01))
  (swiper-line-face (:background trondheim-nord02))
  (swiper-match-face-1 (:inherit 'ivy-current-match))
  (swiper-match-face-2 (:inherit 'ivy-minibuffer-match-face-2))
  (swiper-match-face-3 (:inherit 'ivy-minibuffer-match-face-3))
  (swiper-match-face-4 (:inherit 'ivy-minibuffer-match-face-4))

  ;; magit faces -- taken mostly from Nord
  (magit-diff-context-highlight (:background trondheim-nord01))
  (magit-diff-file-header (:foreground trondheim-nord08 :box (:color trondheim-nord08)))
  (magit-diff-hunk-heading (:foreground trondheim-nord09 :background trondheim-nord01))
  (magit-diff-hunk-heading-highlight (:background trondheim-nord01))
  (magit-diff-added (:foreground trondheim-nord14 :background trondheim-nord02))
  (magit-diff-added-highlight (:foreground trondheim-nord14
                               :background trondheim-nord02
                               :weight 'bold))
  (magit-diff-removed (:foreground trondheim-nord11 :background trondheim-nord02))
  (magit-diff-removed-highlight (:foreground trondheim-nord11
                                 :background trondheim-nord02
                                 :weight 'bold))
  (magit-diffstat-added (:foreground trondheim-nord14))
  (magit-diffstat-removed (:foreground trondheim-nord11))
  (magit-item-highlight (:background trondheim-nord01))
  (magit-hash (:foreground trondheim-comment))
  (magit-log-author (:foreground trondheim-nord09))
  (magit-hunk-heading (:foreground trondheim-nord09 :background trondheim-nord01))
  (magit-hunk-heading-highlight (:background trondheim-nord01))
  (magit-section-heading (:foreground trondheim-nord07 :weight 'bold))
  (magit-section-highlight (:background trondheim-nord01))
  (magit-process-ng (:foreground trondheim-nord13))
  (magit-process-ok (:foreground trondheim-nord14))
  (magit-branch (:foreground trondheim-nord09 :weight 'bold))
  (magit-branch-local (:foreground trondheim-nord09))
  (magit-branch-remote (:foreground trondheim-nord14))

  ;; Fringe indicators for version control
  (git-gutter-fr:added (:foreground trondheim-nord14))
  (git-gutter-fr:deleted (:foreground trondheim-nord11))
  (git-gutter-fr:modified (:foreground trondheim-nord13))

  ;; Relative line numbers faces
  (line-number (:foreground trondheim-nord03))
  (line-number-current-line (:foreground trondheim-nord05))

  ;; rainbow-delimiters mode faces (a color for each depth)
  (rainbow-delimiters-base-error-face (:foreground trondheim-nord11 :weight 'bold))
  (rainbow-delimiters-depth-1-face (:foreground trondheim-nord09))
  (rainbow-delimiters-depth-2-face (:foreground trondheim-nord14))
  (rainbow-delimiters-depth-3-face (:foreground trondheim-nord10))
  (rainbow-delimiters-depth-4-face (:foreground trondheim-nord13))
  (rainbow-delimiters-depth-5-face (:foreground trondheim-nord15))
  (rainbow-delimiters-depth-6-face (:foreground trondheim-nord07))
  (rainbow-delimiters-depth-7-face (:foreground trondheim-nord15))
  (rainbow-delimiters-depth-8-face (:foreground trondheim-nord09))
  (rainbow-delimiters-depth-9-face (:foreground trondheim-nord14))

  ;; lsp specific faces
  (lsp-headerline-breadcrumb-path-face (:foreground trondheim-comment))
  (lsp-headerline-breadcrumb-symbols-face (:foreground trondheim-nord08))
  (lsp-headerline-breadcrumb-separator-face (:inherit 'lsp-headerline-breadcrumb-path-face))

  (lsp-ui-doc-background (:background trondheim-nord03))
  (lsp-ui-doc-header (:background trondheim-nord03 :foreground trondheim-nord08))

  (lsp-ui-sideline-code-action (:foreground trondheim-nord13))

  ;; FIXME: why are :underline styles depending on the :style?
  ;;
  ;; If the style is set to 'wave, the line is rendered right below
  ;; the text; however if the style is 'line it is rendered below the
  ;; box, which is very far from the text in case of the header line
  ;; (due to the custom padding). Is this dependant on the Emacs
  ;; version?

  ;; TODO: change all 'wave for 'line
  ;; (lsp-headerline-breadcrumb-path-error-face
  ;;  (:underline (:style 'wave :color trondheim-nord11)))
  ;; (lsp-headerline-breadcrumb-path-hint-face
  ;;  (:underline (:style 'wave :color trondheim-nord14)))
  ;; (lsp-headerline-breadcrumb-path-info-face
  ;;  (:underline (:style 'wave :color trondheim-nord14)))
  ;; (lsp-headerline-breadcrumb-path-warning-face
  ;;  (:underline (:style 'wave :color trondheim-nord13)))
  ;; (lsp-headerline-breadcrumb-symbol-error-face
  ;;  (:underline (:style 'wave :color trondheim-nord11)))
  ;; (lsp-headerline-breadcrumb-symbol-hint-face
  ;;  (:underline (:style 'wave :color trondheim-nord14)))
  ;; (lsp-headerline-breadcrumb-symbol-info-face
  ;;  (:underline (:style 'wave :color trondheim-nord14)))
  ;; (lsp-headerline-breadcrumb-symbol-warning-face
  ;;  (:underline (:style 'wave :color trondheim-nord13)))


  ;; linting faces
  (flymake-error (:underline (:style 'line :color trondheim-nord11)))
  (flymake-warning (:underline (:style 'line :color trondheim-nord13)))
  (flymake-note (:underline (:style 'line :color trondheim-nord08)))

  (flycheck-error (:underline (:style 'line :color trondheim-nord11)))
  (flycheck-warning (:underline (:style 'line :color trondheim-nord13)))
  (flycheck-info (:underline (:style 'line :color trondheim-nord08)))

  (flyspell-incorrect (:underline (:style 'line :color trondheim-nord11)))
  (flyspell-duplicate (:underline (:style 'line :color trondheim-nord13)))

  ;; circe faces
  (circe-server-face (:foreground trondheim-comment))
  (circe-highlight-nick-face (:foreground trondheim-nord08))
  (circe-originator-face (:foreground trondheim-nord08))
  (circe-prompt-face (:foreground trondheim-nord08 :weight 'bold))
  (circe-fool-face (:foreground trondheim-nord03))
  (lui-button-face (:foreground trondheim-nord08))
  (lui-time-stamp-face (:foreground trondheim-nord15))

  ))


(provide-theme 'trondheim)
;;; trondheim-theme.el ends here
