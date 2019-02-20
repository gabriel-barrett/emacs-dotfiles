;; Prerequisites
(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;;; Speed up init.
;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun custom/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook 'custom/reset-gc-cons-threshold)
;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun custom/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook 'custom/reset-file-name-handler-alist)

;; Packaging config
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Extra config files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
    (progn (add-hook 'display-line-numbers-mode-hook
                     (lambda () (setq display-line-numbers-type 'relative)))
           (global-display-line-numbers-mode))
  (use-package nlinum-relative
    :ensure t
    :config
    (nlinum-relative-setup-evil)
    (setq nlinum-relative-redisplay-delay 0)
    (add-hook 'prog-mode-hook #'nlinum-relative-mode)))

;; Theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn 1))

;;; Packages
;; Ivy and counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

  (use-package ivy
    :ensure t
    :bind (("C-x C-b" . ivy-switch-buffer))
    :config
    (ivy-mode 1))

;; Evil mode
(require 'init-evil)

;; Org mode
(require 'init-org)

;;; Finalization
;; Custom set variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(provide 'init)
