(defun custom/config-evil ()
  "Configure evil mode."

  ;; Use motion state in these additional modes.
  (dolist (mode '(package-menu-mode
                  eww-mode))
    (add-to-list 'evil-motion-state-modes mode))
  (delete 'package-menu-mode evil-emacs-state-modes)

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(dired-mode
                  eshell-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Global bindings.
  (evil-define-key 'normal global-map (kbd ";")     'evil-ex)
  (evil-define-key 'normal global-map (kbd ">")     'evil-repeat-find-char)
  (evil-define-key 'normal global-map (kbd "<")     'evil-repeat-find-char-reverse)
  )


(defun custom/config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","  'other-window
    "."  'mode-line-other-buffer
    ":"  'eval-expression
    "b"  'helm-mini             ;; Switch to another buffer
    "c"  'comment-dwim
    "d"  'kill-this-buffer
    "f"  'helm-find-files
    ;"g"  'magit-status
    "l"  'whitespace-mode       ;; Show invisible characters
    "o"  'delete-other-windows  ;; C-w o
    "p"  'helm-show-kill-ring
    "R"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display))
    "S"  'delete-trailing-whitespace
    "w"  'save-buffer
    "x"  'helm-M-x
    "y"  'yank-to-x-clipboard))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll 1)
  :commands (evil-mode evil-define-key)
  :config

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (custom/config-evil-leader))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (custom/config-evil))

(evil-mode 1)
(provide 'init-evil)
