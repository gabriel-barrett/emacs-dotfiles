;; -*- lexical-binding: t -*-

;; Rust
(custom/add-to-path-if-dir "$HOME/.cargo/bin")
(add-hook 'rust-mode-hook (lambda () (cargo-minor-mode 1)))
(setq-default rust-indent-offset 2)

;; Common Lisp
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
(advice-add 'lean-select-mode :after (lambda (&rest r) (set-input-method "Lean")))

;; Haskell

;; Lurk

;; Erlang

(provide 'langs-init)
