;; -*- lexical-binding: t -*-
;; Measure the init time
(defun custom/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		   (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'custom/display-startup-time)

;; Custom set variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Extra config files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set mail and user name
(setq user-full-name "Gabriel Barreto")
(setq user-mail-address "gabriel.aquino.barreto@gmail.com")

;; Packaging config
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; General settings
(setq inhibit-splash-screen 1
      inhibit-startup-message 1
      inhibit-startup-echo-area-message 1
      visible-bell 1
      column-number-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(electric-pair-mode)

;; Misc
(show-paren-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq enable-local-variables :safe)
(setenv "PAGER" "cat")

;; Transparency function
(defun custom/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Which key
(require 'which-key)
(setq which-key-idle-delay 0.2)
(which-key-mode 1)

;; Basic settings for backup, auto-saves, etc
(setq
   backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
   backup-by-copying 1
   delete-old-versions 1
   version-control 1)

(make-directory
 (expand-file-name "tmp/auto-saves/sessions" user-emacs-directory) t)
(setq
 auto-save-list-file-prefix
 (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
 auto-save-file-name-transforms
 `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq undo-tree-auto-save-history nil)

;; Relative numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Theme
(load-theme 'spacemacs-dark 1)

;; TRAMP
(require 'tramp)
(setq tramp-remote-path
      (append tramp-remote-path
              '(tramp-own-remote-path)))

;; Evil mode
(setq evil-want-keybinding nil
      evil-want-integration t
      evil-respect-visual-line-mode t
      evil-want-C-u-scroll t)
(when (require 'evil nil t) (require 'evil-init))

;; Langs
(require 'langs-init)

;; Icomplete
(fido-vertical-mode 1)

;; Dired
(with-eval-after-load 'dired (require 'dired-init))

;; eww
(with-eval-after-load 'eww (require 'eww-init))

;; Common Lisp
(with-eval-after-load 'common-lisp-mode (require 'cl-init))

;; Rust
(add-hook 'rust-mode-hook (lambda () (cargo-minor-mode 1)))

;; Magit
(require 'magit)

;; Doom modeline
(doom-modeline-mode t)
(setq doom-modeline-icon nil)

;; Popper mode
(require 'popper)
(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        ;; help-mode
        compilation-mode))
(global-set-key (kbd "C-`") 'popper-toggle-latest)  
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(popper-mode +1)

;; For echo-area hints
(require 'popper-echo)
(popper-echo-mode +1)

;; Lean4
(add-to-list 'auto-mode-alist
	     '("\\.lean$" . (lambda ()
			      (require 'lean4-mode)
			      (lean4-mode))))

(provide 'init)
