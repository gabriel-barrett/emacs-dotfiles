;; -*- lexical-binding: t -*-

(defun custom/display-startup-time ()
  "Measure the init time and display it at startup"
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'custom/display-startup-time)

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

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Magit
(use-package magit
  :ensure t)

;; God mode
(use-package god-mode
  :ensure t
  :config
  (global-set-key (kbd "C-'") #'god-mode-all)
  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "C-'") #'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "C-'") #'god-mode-isearch-disable))

;; Icons, etc
(use-package spacemacs-theme
  :ensure t)
(use-package nerd-icons
  :ensure t)
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
(use-package solaire-mode
  :ensure t)

;; Load custom file
(load custom-file 'noerror)

;; Miscellaneous
(put 'dired-find-alternate-file 'disabled nil)
