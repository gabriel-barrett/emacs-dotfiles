;; -*- lexical-binding: t -*-

;; Rust
(add-hook 'rust-mode-hook (lambda () (cargo-minor-mode 1)))
(setq-default rust-indent-offset 2)

;; Lean4
(add-to-list 'load-path (expand-file-name "lean4-mode" user-emacs-directory))
(add-to-list 'auto-mode-alist
	     '("\\.lean$" . (lambda ()
			      (require 'lean4-mode)
			      (lean4-mode))))

;; Common Lisp
(with-eval-after-load 'common-lisp-mode (require 'cl-init))

;; JS and TS
(setq-default typescript-indent-level 2)
(setq-default js-indent-level 2)

(provide 'langs-init)
