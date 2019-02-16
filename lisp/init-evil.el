(setq evil-want-C-u-scroll 1)

(dolist (mode '(package-menu-mode))
    (add-to-list 'evil-motion-state-modes mode))
(delete 'package-menu-mode evil-emacs-state-modes)

(dolist (mode '(ag-mode
                  dired-mode
                  eshell-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes)

(require 'evil)
(evil-mode 1)
(provide 'init-evil)
