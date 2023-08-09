;; -*- lexical-binding: t -*-

;; Eglot
(straight-use-package 'eglot)
(straight-use-package 'company)
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

;; Rust
(custom/add-to-path-if-dir "$HOME/.cargo/bin")
(straight-use-package 'rust-mode)
(straight-use-package 'cargo-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'company-mode)

;; Common Lisp
(straight-use-package 'slime)
(setq inferior-lisp-program "sbcl")
(let ((quicklisp "~/quicklisp/slime-helper.el"))
  (when (file-exists-p quicklisp)
    (load (expand-file-name quicklisp))))
(setq slime-contribs (append slime-contribs '(slime-quicklisp slime-asdf)))
(evil-define-key 'normal slime-repl-mode-map (kbd "g r") 'slime-restart-inferior-lisp)

;; Geiser
(straight-use-package 'geiser)
(straight-use-package 'geiser-chez)
(straight-use-package 'geiser-guile)
(straight-use-package 'geiser-racket)

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

;; OCaml
(straight-use-package 'caml-mode)
(straight-use-package 'tuareg)
(straight-use-package 'dune)
(straight-use-package 'utop)
(straight-use-package 'flycheck-ocaml)
(custom/add-to-path-if-dir "$HOME/.opam/default/bin")
(add-hook 'tuareg-mode-hook #'utop-minor-mode)
(add-hook 'tuareg-mode-hook #'eglot-ensure)
(add-hook 'tuareg-mode-hook #'company-mode)
(add-to-list 'auto-mode-alist '("\\.ocamlinit\\'" . tuareg-mode))
(setq utop-command "opam exec -- dune utop . -- -emacs")
(setq tuareg-interactive-program "opam exec -- dune utop . -- -emacs")

;; Lurk
(straight-use-package 'lurk-mode)

;; Erlang
(straight-use-package 'erlang)

;; Nix
(straight-use-package 'nix-mode)

(provide 'langs-init)
