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

(define-derived-mode formality-mode prog-mode "Formality"
  "Major mode for editing Formality programs."
  :group 'formality

  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "\\(@[[:alpha:]]+\\>\\|$\\)"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-line-break-function #'c-indent-new-comment-line)
  (setq-local c-block-comment-start-regexp "/\\*")
  (setq-local comment-multi-line t)

  (local-set-key (kbd "C-c C-l") (lambda () (interactive) (async-shell-command "/home/nixos/.npm-global/bin/fm")))
  )

(provide 'formality-mode)
