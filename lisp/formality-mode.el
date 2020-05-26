(defconst formality-version "0.1"
  "The release version of `formality-mode'.")

(defun formality-version (&optional here)
  "Show the `formality-mode` version in the echo area."
  (interactive "P")
  (let* ((formality-mode-dir (ignore-errors
                             (file-name-directory (or (locate-library "formality-mode") ""))))
         (version (format "formality-mode version %s (%s)"
                           formality-version
                           formality-mode-dir)))
    (if here
        (insert version)
      (message "%s" version))))

(add-to-list 'auto-mode-alist '("\\.fm\\'" . formality-mode))
(add-to-list 'auto-mode-alist '("\\.fmc\\'" . formality-mode))

(defface formality-number-face
  '((t :foreground "purple"))
  "Face for numbers."
  :group 'formality)

(defvar formality-number-face 'formality-number-face)

(define-derived-mode formality-mode prog-mode "Formality"
  "Major mode for editing Formality programs."
  :group 'formality

  (setq-local comment-start "//")
  (setq-local comment-end "")
  (font-lock-add-keywords 'nil '(("//.*" 0 font-lock-comment-face t)
                                 ("Nat\.[0-9]+" . 'formality-number-face)
                                 (":\\|;\\|=\\|->\\|<\\|>\\|,\\|(\\|)\\||" . 'font-lock-variable-name-face)
                                 ("let" . font-lock-keyword-face)
                                 ("get" . font-lock-keyword-face)))
  (local-set-key (kbd "C-c C-l")
                 (lambda () (interactive) (async-shell-command "/home/nixos/.npm-global/bin/fm")))
  )

(provide 'formality-mode)
