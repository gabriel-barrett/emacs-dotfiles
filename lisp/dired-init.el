;; -*- lexical-binding: t -*-
(defun custom/dired-touch ()
  "Create a new, empty file."
  (interactive)
  (let ((name (read-string "Create file:")))
    (if (file-exists-p name)
        (message (concat "File " name " already exists."))
      (progn (shell-command (concat "touch \"" name "\""))
             (revert-buffer)
             (message (concat "File " name " created."))))))

(evil-define-key 'normal dired-mode-map
  (kbd "T") 'custom/dired-touch
  (kbd ";") 'with-editor-async-shell-command)

(straight-use-package 'openwith)
(require 'openwith)

(put 'dired-find-alternate-file 'disabled nil)

(setq openwith-associations
      `((,(rx "." (or "mp4" "mkv" "webm" "avi" "flv" "mov" "mpeg" "wmv")) "mpv"
	(file))
       (,(rx "." (or "jpg" "jpeg" "png")) "feh"
	("--fullscreen" file))))

(defun abort-if-file-too-large--except (orig-fn size op-type filename &optional offer-raw)
  "Do not abort if FILENAME is handled by Openwith."
  (let ((ok-large-file-types (mapconcat 'car openwith-associations "\\|")))
    (unless (string-match-p ok-large-file-types filename)
      (funcall orig-fn size op-type filename offer-raw))))
(advice-add 'abort-if-file-too-large :around 'abort-if-file-too-large--except)

(openwith-mode +1)
(setq dired-listing-switches "-alGh --group-directories-first")

(provide 'dired-init)
