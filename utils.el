;;; ~/.doom.d/utils.el -*- lexical-binding: t; -*-

(defun dvm/quit-window-dwim (&optional args)
  "`delete-frame' if single window, else `quit-window' and ARGS."
  (interactive)
  (if (one-window-p)
      (delete-frame)
    (quit-window args)))

(defun dvm/adapt-quit-window-for-mode (mode)
  "Change how quit-window works on `mode' for i3."
  (substitute-key-definition 'quit-window 'dvm/quit-window-dwim mode))
