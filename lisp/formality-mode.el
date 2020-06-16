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

(defvar formality-mode-syntax-table nil "Syntax table for `formality-mode'.")

(setq formality-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
        (modify-syntax-entry ?/ ". 12b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        synTable))

(defface formality-number-face
  '((t :foreground "purple"))
  "Face for numbers."
  :group 'formality)

(defvar formality-number-face 'formality-number-face)

(defvar formality-symbols (append ":|;(),<> " nil))

(define-derived-mode formality-mode prog-mode "Formality"
  "Major mode for editing Formality programs."
  :group 'formality

  (set-syntax-table formality-mode-syntax-table)

  ; For comment-dwim
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (font-lock-add-keywords 'nil '(;("[0-9]+" . 'font-lock-constant-face)
                                 ("let \\([^=]+?\\)=" . (1 font-lock-function-name-face))
                                 ("def \\([^=]+?\\)=" . (1 font-lock-function-name-face))
                                 ("use \\([^=]+?\\)=" . (1 font-lock-function-name-face))
                                 ("get \\([^=]+?\\)=" . (1 font-lock-function-name-face))
                                 (":\\|;\\|=\\|->\\|<\\|>\\|,\\|(\\|)\\||" . 'font-lock-variable-name-face)
                                 ("let " . font-lock-keyword-face)
                                 ("def " . font-lock-keyword-face)
                                 ("use " . font-lock-keyword-face)
                                 ("get " . font-lock-keyword-face)
                                 ("case " . font-lock-keyword-face)
                                 ("\'.*\'" . font-lock-string-face)
                                 ))


  (local-set-key (kbd "C-c C-l")
                 (lambda ()
                   "Type checks fm files at current folder."
                   (interactive)
                   ;; (async-shell-command "~/.npm-global/bin/fm")))
                   (async-shell-command "~/Job/Formality/javascript/bin/fm.js")))
  (local-set-key (kbd "C-c C-c")
                 (lambda (code)
                 "Compiles a formality top-level term to JS."
                 ;; There must be a more efficient way to get this
                 (interactive
                  (let ((term (seq-take-while
                               (lambda (chr) (not (member chr formality-symbols)))
                               (seq-drop-while
                                (lambda (chr) (member chr formality-symbols))
                                (thing-at-point 'line t)))))
                    (list
                     (read-string (format "word (%s): " term)
                                  nil nil term))))
                   (async-shell-command (concat "~/Job/Formality/javascript/bin/fmcjs.js " code))))
  )

(provide 'formality-mode)
