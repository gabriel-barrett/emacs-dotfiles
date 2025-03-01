;; -*- lexical-binding: t -*-

;; Eglot
(require 'eglot)
(straight-use-package 'company)
(straight-use-package 'eldoc-box)
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
(setq slime-contribs (append slime-contribs '(slime-quicklisp slime-asdf slime-repl)))
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
(add-to-list 'eglot-server-programs
             '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
(custom/add-to-path-if-dir "$HOME/.ghcup/bin")
(custom/add-to-path-if-dir "$HOME/.cabal/bin")
(defun custom/haskell-config-hook ()
  "Configuration hook for Haskell mode."
  (add-hook 'before-save-hook #'eglot-format)
  (eglot-ensure)
  (company-mode))
(add-hook 'haskell-mode-hook #'custom/haskell-config-hook)

;; OCaml
(straight-use-package 'tuareg)
(straight-use-package 'dune)
(straight-use-package 'utop)
(straight-use-package 'flycheck-ocaml)
(straight-use-package 'merlin)
(straight-use-package 'ocamlformat)

(defun custom/tuareg-config-hook ()
  "Configuration hook for Tuareg mode."
  (eglot-ensure)
  (company-mode)
  (utop-minor-mode)
  (merlin-mode)
  (flycheck-mode)
  (setq-local
   merlin-command 'opam
   compile-command (concat "dune build")
   compilation-read-command nil))

(with-eval-after-load 'tuareg
  (define-key tuareg-mode-map (kbd "C-c C-f") 'ocamlformat)
  (define-key tuareg-mode-map (kbd "C-c C-t") 'merlin-type-enclosing)
  (define-key tuareg-mode-map (kbd "C-c C-l") 'utop-eval-buffer)
  (define-key tuareg-mode-map (kbd "C-c C-c") 'compile))

(custom/add-to-path-if-dir "$HOME/.opam/default/bin")
(add-hook 'tuareg-mode-hook #'custom/tuareg-config-hook)
(add-to-list 'auto-mode-alist '("\\.ocamlinit\\'" . tuareg-mode))
(setq utop-command "opam exec -- dune utop . -- -emacs")
(setq tuareg-interactive-program "opam exec -- dune utop . -- -emacs")

;; Erlang
(straight-use-package 'erlang)

;; Nix
(straight-use-package 'nix-mode)

;; Zig
(straight-use-package 'zig-mode)
(add-to-list 'eglot-server-programs '(zig-mode . ("zls")))

(provide 'langs-init)
