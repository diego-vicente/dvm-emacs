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

(defun dvm/workspace-associate-frame-fn (frame &optional _new-frame-p)
  "Modified version of `+workspaces-associate-frame-fn'

This function is used to add all automatically created buffers
to `+workspaces-main', instead of creating new ones."
  (when persp-mode
    (if (not (persp-frame-list-without-daemon))
        (+workspace-switch +workspaces-main t)
      (with-selected-frame frame
        (+workspace-switch +workspaces-main t)
        (unless (doom-real-buffer-p (current-buffer))
          (switch-to-buffer (doom-fallback-buffer)))
        (set-frame-parameter frame 'workspace (+workspace-current-name))
        ;; ensure every buffer has a buffer-predicate
        (persp-set-frame-buffer-predicate frame))
      (run-at-time 0.1 nil #'+workspace/display))))
