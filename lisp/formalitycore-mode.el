(defconst formalitycore-version "0.1"
  "The release version of `formalitycore-mode'.")

(defun formalitycore-version (&optional here)
  "Show the `formalitycore-mode` version in the echo area."
  (interactive "P")
  (let* ((formalitycore-mode-dir (ignore-errors
                               (file-name-directory (or (locate-library "formalitycore-mode") ""))))
         (version (format "formalitycore-mode version %s (%s)"
                          formalitycore-version
                          formalitycore-mode-dir)))
    (if here
        (insert version)
      (message "%s" version))))

(add-to-list 'auto-mode-alist '("\\.fmc\\'" . formalitycore-mode))

(defvar formalitycore-mode-syntax-table nil "Syntax table for `formalitycore-mode'.")

(setq formalitycore-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
        (modify-syntax-entry ?/ ". 12b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        synTable))

(defcustom formalitycore-dir "~/.npm-global/lib/node_modules/formality-lang/bin/" "Directory in which FormalityCore is installed.")

(defvar formalitycore-symbols (append ":|;(),<> " nil))

(define-derived-mode formalitycore-mode prog-mode "FormalityCore"
  "Major mode for editing FormalityCore programs."
  :group 'formalitycore

  (set-syntax-table formalitycore-mode-syntax-table)

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
                   "Type checks fm files at current directory."
                   (interactive)
                   (async-shell-command (concat formalitycore-dir "fmc.js"))))
  
  )

(provide 'formalitycore-mode)
