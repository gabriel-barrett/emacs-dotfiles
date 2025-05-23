;; Rust
(use-package cargo-mode :ensure t)
(use-package company :ensure t)
(use-package rust-mode :ensure t
  :config
  (custom/add-to-path-if-dir "$HOME/.cargo/bin")
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'company-mode))

;; Lean
(use-package dash :ensure t)
(use-package lsp-mode :ensure t)
(use-package magit-section :ensure t)
(use-package lean4-mode
  :custom
  (lean4-delete-trailing-whitespace t)
  :commands lean4-mode
  :config
  (custom/add-to-path-if-dir "$HOME/.elan/bin")
  :vc (:url "https://github.com/leanprover-community/lean4-mode.git"
	    :rev :last-release))

;; Miscellaneous
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

(provide 'langs)
