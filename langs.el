;; Eglot
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

;; Company, etc
(use-package company :defer t)
(use-package paredit :defer t)
(use-package flycheck :defer t)

;; Common lisp
(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy))
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'company-mode))

;; Rust
(use-package rust-mode
  :defer t
  :hook
  (rust-mode . cargo-minor-mode)
  (rust-mode . eglot-ensure)
  (rust-mode . company-mode)
  (before-save-hook . eglot-format-buffer)
  :config
  (custom/add-to-path-if-dir "$HOME/.cargo/bin"))
(use-package cargo-mode
  :after rust-mode)

;; Lean
(use-package dash)
(use-package lsp-mode
  :defer t)
(use-package magit-section
  :defer t)
(use-package lean4-mode
  :defer t
  :custom
  (lean4-delete-trailing-whitespace t)
  :commands lean4-mode
  :config
  (custom/add-to-path-if-dir "$HOME/.elan/bin")
  :vc (:url "https://github.com/leanprover-community/lean4-mode.git"
	    :rev :last-release))

(provide 'langs)
