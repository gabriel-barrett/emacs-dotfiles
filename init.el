;; -*- lexical-binding: t -*-

(defun custom/display-startup-time ()
  "Measure the init time and display it at startup"
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'custom/display-startup-time)

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

;; Load custom file
(load custom-file 'noerror)
