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
  (evil-define-key 'normal global-map (kbd ":") 'evil-command-window-ex)
  (evil-define-key 'normal global-map (kbd ";") 'async-shell-command)
  (evil-define-key 'normal global-map (kbd ">") 'evil-repeat-find-char)
  (evil-define-key 'normal global-map (kbd "<") 'evil-repeat-find-char-reverse)
  )

(defun custom/config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","  'other-window
    "r"  'revert-buffer
    "h"  'evil-window-left
    "l"  'evil-window-right
    "TAB"  'other-window
    "."  'mode-line-other-buffer
    "b"  'helm-mini
    "c"  'comment-dwim
    "d"  'kill-this-buffer
    "D"  'dired
    "k"  'kill-buffer
    "f"  'helm-find-files
    "g"  'magit-status
    "o"  'delete-other-windows
    "p"  'helm-show-kill-ring
    "w"  'save-buffer
    "x"  'helm-M-x))


(evil-mode t)
(custom/config-evil)

(evil-collection-init)
(setq evil-collection-setup-minibuffer t)

(global-evil-leader-mode)
(custom/config-evil-leader)
(global-evil-surround-mode)
(with-current-buffer "*Messages*" (evil-leader-mode))

(provide 'evil-init)
