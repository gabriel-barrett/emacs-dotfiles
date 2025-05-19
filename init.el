;; -*- lexical-binding: t -*-

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
  (let ((expanded-path (substitute-in-file-name path)))
    (when (file-directory-p expanded-path)
      (unless (member expanded-path exec-path)
	(setq exec-path (append exec-path (list expanded-path)))
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
		  (scroll-down-line arg)
		  (previous-line arg)))
(global-set-key (kbd "M-n")
		(lambda (arg)
		  (interactive "p")
		  (scroll-up-line arg)
		  (next-line arg)))
(define-key global-map [remap exchange-point-and-mark]
	    (lambda ()
	      (interactive)
	      (exchange-point-and-mark (not mark-active))))

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Magit
(use-package magit :ensure t)

;; Composable editing
(use-package composable :ensure t
  :config
  (define-key global-map [remap kill-region] #'composable-kill-region)
  (define-key global-map [remap kill-ring-save] #'composable-kill-ring-save)
  (define-key global-map [remap delete-region] #'composable-delete-region)
  (define-key global-map [remap indent-region] #'composable-indent-region)
  (define-key global-map [remap comment-dwim] #'composable-comment-dwim)
  (define-key global-map [remap downcase-region] #'composable-downcase-region)
  (define-key global-map [remap upcase-region] #'composable-upcase-region)
  (setq composable-repeat nil))

;; God mode
(use-package god-mode :ensure t
  :config
  (global-set-key (kbd "C-'") #'god-mode-all)
  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "C-'") #'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "C-'") #'god-mode-isearch-disable))

;; Icons, etc
(use-package spacemacs-theme :ensure t)
(use-package nerd-icons :ensure t)
(use-package doom-modeline :ensure t
  :hook (after-init . doom-modeline-mode))
(use-package solaire-mode :ensure t)

(load (expand-file-name "langs.el" user-emacs-directory))
(require 'langs)

;; Load custom file
(load custom-file 'noerror)

;; Miscellaneous
(put 'dired-find-alternate-file 'disabled nil)
(set-face-attribute 'default nil :height 144)
