;; Packages
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Custom set variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use package
(eval-when-compile
  (require 'use-package))

;; General settings
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq visible-bell t)
(setq column-number-mode 1)
(load-theme 'zenburn 1)

;; Misc
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Basic settings for backup
(setq
   backup-by-copying 1
   backup-directory-alist '(("." . "~/.saves"))
   delete-old-versions 1
   kept-new-versions 6
   kept-old-versions 2
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

;; Ido and Smex (if not using helm)
;(global-set-key (kbd "M-x") 'smex)
;(global-set-key (kbd "<menu>") 'smex)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching 1)
;(require 'ido-completing-read+)
;(ido-ubiquitous-mode 1)
;(setq ido-auto-merge-work-directories-length -1)

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
