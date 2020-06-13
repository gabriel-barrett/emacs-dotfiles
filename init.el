;; Prerequisites
(let ((minver "26.1"))
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

;; Load newer source as opposed to older bytecode
(setq load-prefer-newer t)

;; Extra config files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set mail and user name
(setq user-full-name "Gabriel Barreto")
(setq user-mail-address "gabriel.aquino.barreto@gmail.com")
;; (setq smtpmail-smtp-server "your.smtp.server.jp")
;; (setq mail-user-agent 'message-user-agent)
;; (setq message-send-mail-function 'message-smtpmail-send-it)

;; Packaging config
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

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
(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark 1))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;; Langs
(require 'init-langs)

;; Evil mode
(require 'init-evil)

;; Shell
(defun my-shell-mode-hook ()
  (setq comint-input-ring-file-name "~/.zsh_history")
  (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);") ; remove timestamp
  (comint-read-input-ring t))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;; Helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)
  (define-key helm-read-file-map (kbd "C-k")  'helm-find-files-up-one-level))
(helm-mode 1)

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (require 'yasnippet)
  (yas-global-mode 1))

;; Dired
(require 'init-dired)

;; eww
(require 'init-eww)

;; Mu4e
(let ((mu4e-dir "/etc/profiles/per-user/nixos/share/emacs/site-lisp/mu4e"))
  (if (file-exists-p (concat mu4e-dir "/mu4e.el"))
      (progn
        (add-to-list 'load-path mu4e-dir)
        (require 'mu4e)
        (require 'init-mu4e))
    (message "Mu4e not found.")))

;; Org mode
(require 'init-org)

;; Formality mode
(require 'formality-mode)

;; Agda mode
(let* ((agda-mode-file (shell-command-to-string "command -v agda-mode >/dev/null && agda-mode locate"))
       (coding-system-for-read 'utf-8))
  (if (string-empty-p agda-mode-file)
      (message "Agda mode not found.")
    (load-file agda-mode-file)))

;; Common Lisp
(require 'init-cl)

;;; Finalization
;; Custom set variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(provide 'init)
