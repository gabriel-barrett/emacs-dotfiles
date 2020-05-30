(defun custom/config-evil ()
  "Configure evil mode."

  ;; Use insert state in these additional modes.
  (dolist (mode '(evil-command-window-mode))
    (add-to-list 'evil-insert-state-modes mode))

  ;; Use normal state in these additional modes.
  (dolist (mode '(shell-mode
                  slime-repl-mode
                  geiser-repl-mode))
    (add-to-list 'evil-normal-state-modes mode))
  (delete 'shell-mode evil-insert-state-modes)
  (delete 'slime-repl-mode evil-insert-state-modes)
  (delete 'geiser-repl-mode evil-insert-state-modes)

  ;; Global bindings.
  (evil-define-key 'normal global-map (kbd ":")     'evil-command-window-ex)
  (evil-define-key 'normal global-map (kbd ";")     'async-shell-command)
  (evil-define-key 'normal global-map (kbd ">")     'evil-repeat-find-char)
  (evil-define-key 'normal global-map (kbd "<")     'evil-repeat-find-char-reverse)
  )

(defun custom/config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","  'other-window
    "TAB"  'other-window
    "."  'mode-line-other-buffer
    ":"  'eval-expression
    "b"  'helm-mini             ;; Switch to another buffer
    "c"  'comment-dwim
    "d"  'kill-this-buffer
    "k"  'kill-buffer
    "f"  'helm-find-files
    "g"  'magit-status
    "l"  'whitespace-mode       ;; Show invisible characters
    "o"  'delete-other-windows  ;; C-w o
    "p"  'helm-show-kill-ring
    "R"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display))
    "S"  'delete-trailing-whitespace
    "w"  'save-buffer
    "x"  'helm-M-x))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll 1)
  :commands (evil-mode evil-define-key)
  :config

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (custom/config-evil-leader))

  (use-package evil-magit
    :ensure t
    :config
    (evil-magit-init))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (custom/config-evil))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(evil-mode 1)

(provide 'init-evil)
