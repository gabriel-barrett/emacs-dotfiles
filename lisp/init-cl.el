(use-package slime
  :ensure t
  :config
  (let ((quicklisp "~/.quicklisp/slime-helper.el"))
    (when (file-exists-p quicklisp)
      (load (expand-file-name quicklisp))))
  (setq inferior-lisp-program "sbcl")
  ;; Custom keybindings
  (evil-define-key 'normal slime-repl-mode-map (kbd "g r") 'slime-restart-inferior-lisp)
  )

(provide 'init-cl)
