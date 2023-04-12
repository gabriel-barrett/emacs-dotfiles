;; -*- lexical-binding: t -*-

;; Rust
(custom/add-to-path-if-dir "$HOME/.cargo/bin")
(straight-use-package 'rust-mode)
(add-hook 'rust-mode-hook (lambda () (cargo-minor-mode 1)))
(setq-default rust-indent-offset 2)

;; Common Lisp
(straight-use-package 'slime)
(defun custom/config-cl ()
  "Configure common lisp mode"
  (let ((quicklisp "~/.quicklisp/slime-helper.el"))
    (when (file-exists-p quicklisp)
      (load (expand-file-name quicklisp))))
  (setq inferior-lisp-program "sbcl")
  ;; Custom keybindings
  (evil-define-key 'normal slime-repl-mode-map (kbd "g r") 'slime-restart-inferior-lisp))
(with-eval-after-load 'common-lisp-mode (custom/config-cl))


;; JS and TS
(setq-default typescript-indent-level 2)
(setq-default js-indent-level 2)

;; Lean
(custom/add-to-path-if-dir "$HOME/.elan/bin")
(straight-use-package '(lean4-mode
			:type git
			:host github
			:repo "leanprover/lean4-mode"
			:files ("*.el" "data")))
(advice-add 'lean-select-mode :after (lambda (&rest r) (set-input-method "Lean")))

;; Haskell
(straight-use-package 'haskell-mode)

;; Lurk
(straight-use-package 'lurk-mode)

;; Erlang
(straight-use-package 'erlang)

(provide 'langs-init)
