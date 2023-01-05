(defun custom/display-message-in-buffer (message buffer-name &optional action frame)
  "Display message in a buffer."
  (with-current-buffer
      (get-buffer-create buffer-name)
    (erase-buffer)
    (insert message)
    (goto-char (point-min))
    (display-buffer (current-buffer) action frame)))

(defun custom/flycheck-display-error-messages (errors)
  "Alternative display error function that never displays in the echo area."
  (when (and errors (flycheck-may-use-echo-area-p))
    (let ((message (flycheck-help-echo-all-error-messages errors)))
      (custom/display-message-in-buffer
       message flycheck-error-message-buffer 'not-this-window)
      (-when-let ((buf (get-buffer flycheck-error-message-buffer)))
        (with-current-buffer buf
          (unless (derived-mode-p 'flycheck-error-message-mode)
            (flycheck-error-message-mode)))))))

(setq flycheck-display-errors-delay 0.3)
(setq flycheck-display-errors-function #'custom/flycheck-display-error-messages)

(provide 'flycheck-init)
