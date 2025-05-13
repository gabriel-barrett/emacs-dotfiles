;; Rust
(custom/add-to-path-if-dir "$HOME/.cargo/bin")
(use-package rust-mode :ensure t)
(use-package cargo-mode :ensure t)
(use-package company :ensure t)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'company-mode)

;; Lean
(custom/add-to-path-if-dir "$HOME/.elan/bin")
(use-package dash :ensure t)
(use-package lsp-mode :ensure t)
(use-package magit-section :ensure t)
(use-package lean4-mode
  :commands lean4-mode
  :vc (:url "https://github.com/leanprover-community/lean4-mode.git"
       :rev :last-release))
(require 'lean4-mode)

(provide 'langs)
