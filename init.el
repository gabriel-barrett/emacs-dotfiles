;; -*- lexical-binding: t -*-

;; Load custom file
(load custom-file 'noerror)

(defun custom/display-startup-time ()
  "Measure the init time and display it at startup"
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'custom/display-startup-time)

(defun custom/add-to-path-if-dir (path)
  "Add PATH to the `exec-path` and `PATH` environment variable if it is a valid directory."
  (let ((expanded-path (expand-file-name path)))
    (when (file-directory-p expanded-path)
      (unless (member expanded-path exec-path)
	(add-to-list 'exec-path expanded-path)
	(setenv "PATH" (concat (getenv "PATH") path-separator expanded-path))))))

;; Window management
(global-set-key (kbd "s-<tab>") #'other-window)
(global-set-key (kbd "s-t") #'window-swap-states)
(global-set-key (kbd "s-q") #'delete-window)
(global-set-key (kbd "s-h") #'split-window-right)
(global-set-key (kbd "s-v") #'split-window-below)

;; Buffer management
(global-set-key (kbd "s-,") #'previous-buffer)
(global-set-key (kbd "s-.") #'next-buffer)
(define-key global-map [remap list-buffers] #'ibuffer)

;; ISearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; Shell
(global-set-key (kbd "C-c m") #'eshell)
(global-set-key (kbd "C-c M") (lambda () (interactive) (eshell t)))

;; Other keybindings
(global-set-key (kbd "C-.") #'repeat)
(global-set-key (kbd "M-Z") #'zap-up-to-char)
(global-set-key (kbd "M-p")
		(lambda (arg)
		  (interactive "p")
		  (ignore-errors (scroll-down-line arg))
		  (previous-line arg)))
(global-set-key (kbd "M-n")
		(lambda (arg)
		  (interactive "p")
		  (ignore-errors (scroll-up-line arg))
		  (next-line arg)))
(define-key global-map [remap exchange-point-and-mark]
	    (lambda ()
	      (interactive)
	      (exchange-point-and-mark (not mark-active))))
(defvar custom/night-mode nil)
(global-set-key (kbd "s-n")
		(lambda () (interactive)
		  (if custom/night-mode
		      (progn (set-frame-parameter (selected-frame) 'alpha 100) (load-theme 'modus-operandi-tinted))
		    (progn (set-frame-parameter (selected-frame) 'alpha 96) (load-theme 'modus-vivendi-tinted)))
		  (setq custom/night-mode (not custom/night-mode))))

;; Packages
(require 'package)
(require 'use-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Org mode configuration
(use-package org
  :ensure nil
  :defer t
  :custom (org-fontify-whole-heading-line t)
  :custom-face
  (org-level-1 ((t (:height 1.4))))
  (org-level-2 ((t (:height 1.2))))
  (org-level-3 ((t (:height 1.1)))))

;; Magit and Forge
(use-package magit
  :defer t)
(use-package forge
  :after magit)

;; Composable editing
(use-package composable
  :config
  (define-key global-map [remap kill-region] #'composable-kill-region)
  (define-key global-map [remap kill-ring-save] #'composable-kill-ring-save)
  (define-key global-map [remap delete-region] #'composable-delete-region)
  (define-key global-map [remap indent-region] #'composable-indent-region)
  (define-key global-map [remap comment-dwim] #'composable-comment-dwim)
  (define-key global-map [remap downcase-region] #'composable-downcase-region)
  (define-key global-map [remap upcase-region] #'composable-upcase-region)
  (setq composable-repeat nil))

;; Icons, etc
(use-package nerd-icons)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Podman
(use-package docker
  :defer t
  :bind ("C-c d" . docker)
  :config
  (setf docker-command "podman"
	docker-compose-command "podman-compose"
	docker-container-tramp-method "podman"))

;; God mode
(use-package god-mode
  :custom
  (god-mode-alist '((nil . "C-") ("g" . "M-") ("," . "C-M-")))
  :bind
  (("C-'" . god-mode-all)
   :map isearch-mode-map
   ("C-'" . god-mode-isearch-activate)
   :map god-mode-isearch-map
   ("C-'" . god-mode-isearch-disable))
  :config
  (require 'god-mode-isearch))

;; GPT.el
(use-package gptel
  :defer t
  :init
  (defvar gptel-chat-directory (expand-file-name "gptel-chats/" user-emacs-directory))
  (unless (file-exists-p gptel-chat-directory)
    (make-directory gptel-chat-directory t))
  :custom
  (gptel-prompt-prefix-alist '((markdown-mode . "# ") (org-mode . "* ") (text-mode . "# ")))
  :bind
  (("C-c g" . gptel)
   ("C-c G" . (lambda () (interactive) (find-file (read-file-name "Find file: " gptel-chat-directory)))))
  :config
  (setq gptel-model   'deepseek-chat
	gptel-backend (gptel-make-deepseek "DeepSeek"
			:stream t
			:key gptel-api-key))
  (setq gptel--system-message "You are a helpful assistant for programming living inside Emacs")
  (setq gptel-default-mode 'org-mode)
  (setq gptel-temperature 0.0)
  (add-hook 'gptel-mode-hook (lambda () (interactive) (setq default-directory gptel-chat-directory)))
  (add-hook 'gptel-mode-hook (lambda () (setq truncate-lines nil))))

(load (expand-file-name "langs.el" user-emacs-directory))
(require 'langs)

;; Miscellaneous
(put 'dired-find-alternate-file 'disabled nil)
(set-face-attribute 'default nil :height 120)

(provide 'init)
