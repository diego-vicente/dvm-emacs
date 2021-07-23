;;; dvm-functions.el --- My very own Emacs Lisp functions.
;;; Commentary:

;;; This file contains some basic functions definitions. These are
;;; mostly text-editing or interactive commands: general purpose
;;; functions are defined in the `dvm-utils.el` file and the bindigns
;;; for these functions are most likely defined elsewhere.

;;; Code:

(defun dvm/clean-buffer-dwim (&optional start end)
  "Remove all tabs and trailign whitespaces in the current buffer."
  (interactive)
  (let ((start (or start (point-min)))
        (end   (or end (point-max))))
    (untabify start end)
    (delete-trailing-whitespace start end)))

(defun dvm/apply-to-dired-marked-files (format-function)
  "Map a buffer `FORMAT-FUNCTION' to all files marked in dired."
  (interactive "aFormat function: ")
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (funcall format-function (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))


(defun dvm/read-gpg-secret (password-file)
  "Get the decrypted contents of `PASSWORD-FILE'."
  (interactive "fSecret file: ")
  (string-trim
   (shell-command-to-string
    (format "gpg --decrypt %s 2> /dev/null"
            password-file))))


(defun dvm/quit-window-advice (original-fn &rest args)
  "Quit a buffer and delete the window if needed."
  (let ((last-window (one-window-p)))
    (apply original-fn args)
    (when last-window
      (delete-frame))))


(defmacro dvm/setq-hook (hook variable value)
  "Set a `VARIABLE' to `VALUE' when `HOOK' is run."
  (let* ((fn-name (concat "set-local-" (symbol-name variable)))
         (fn-symbol (intern fn-name)))
    `(progn
       (defun ,fn-symbol ()
         (setq-local ,variable ,value))
       (add-hook ',hook ',fn-symbol))))


(defvar dvm/negate-keyword-alist
  '(("false" . "true")
    ("0" . "1")
    ("nil" . "t"))
  "A list of keyword pairs with opposite meanings.")

(defun dvm/negate-word (word &optional word-alist)
  "Return the `WORD' negated as per `WORD-ALIST'."
  (or word-alist (setq word-alist dvm/negate-keyword-alist))
  (or (cdr (assoc word word-alist))
      (car (rassoc word word-alist))))


(provide 'dvm-functions)
;;; dvm-functions.el ends here
