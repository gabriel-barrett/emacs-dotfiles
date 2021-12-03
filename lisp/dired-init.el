(defun custom/dired-touch ()
  "Create a new, empty file."
  (interactive)
  (let ((name (read-string "Create file:")))
    (if (file-exists-p name)
        (message (concat "File " name " already exists."))
      (progn (shell-command (concat "touch \"" name "\""))
             (revert-buffer)
             (message (concat "File " name " created."))))))

(evil-define-key 'normal dired-mode-map (kbd "T") 'custom/dired-touch)

(provide 'dired-init)
