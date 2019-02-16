(defun air--config-evil ()
  "Configure evil mode."
  (dolist (mode '(package-menu-mode))
    (add-to-list 'evil-motion-state-modes mode))
  (delete 'package-menu-mode evil-emacs-state-modes)

  (dolist (mode '(dired-mode
                  eshell-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll 1)
  :commands (evil-mode evil-define-key)
  :config
  (air--config-evil))

(evil-mode 1)
(provide 'init-evil)
