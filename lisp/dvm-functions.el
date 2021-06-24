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


(provide 'dvm-functions)
;;; dvm-functions.el ends here
