;; Packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Extra config files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Custom set variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; General settings
(setq inhibit-splash-screen 1
      inhibit-startup-message 1
      inhibit-startup-echo-area-message 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(setq visible-bell 1)
(setq column-number-mode 1)
(load-theme 'zenburn 1)

;; Misc
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Basic settings for backup
(defvar backup-dir "~/.emacs.d/backups/")
(setq
   backup-directory-alist `(("." . ,backup-dir))
   backup-by-copying 1
   delete-old-versions 1
   version-control 1)

;; Relative numbers
(if (functionp 'display-line-numbers-mode)
    (and (add-hook 'display-line-numbers-mode-hook
                   (lambda () (setq display-line-numbers-type 'relative)))
         (add-hook 'prog-mode-hook #'display-line-numbers-mode))
  (use-package nlinum-relative
    :ensure t
    :config
    (nlinum-relative-setup-evil)
    (setq nlinum-relative-redisplay-delay 0)
    (add-hook 'prog-mode-hook #'nlinum-relative-mode)))

;; Ido
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching 1)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Evil mode
(require 'init-evil)

;; Org mode
(require 'latex-pretty-symbols)
(add-hook 'org-mode-hook 'latex-unicode-simplified)


;; Open .v files with Proof General's Coq mode
(load "~/.emacs.d/lisp/PG/generic/proof-site")

(provide 'init)
