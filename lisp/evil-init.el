;; -*- lexical-binding: t -*-
(defun custom/config-evil ()
  "Configure Evil mode."

  ;; Use insert state in these additional modes.
  (dolist (mode '(evil-command-window-mode))
    (evil-set-initial-state mode 'insert))

  ;; Use normal state in these additional modes.
  (dolist (mode '(shell-mode)) ;; shell-mode for async processes
    (evil-set-initial-state mode 'normal))

  ;; Use emacs state in these additional modes.
  (dolist (mode '(slime-repl-mode
                  term-mode
                  geiser-repl-mode))
    (evil-set-initial-state mode 'emacs))

  ;; Global bindings.
  (evil-define-key 'normal global-map
    (kbd ":") 'evil-command-window-ex
    (kbd ";") 'with-editor-async-shell-command
    (kbd ">") 'evil-repeat-find-char
    (kbd "<") 'evil-repeat-find-char-reverse
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line)
  )

(defun custom/config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","  'other-window
    "."  'mode-line-other-buffer
    "D"  (lambda () (interactive) (dired "."))
    "b"  'switch-to-buffer
    "c"  'comment-dwim
    "d"  'kill-this-buffer
    "e"  'eldoc-box-help-at-point
    "f"  'find-file
    "g"  'magit-status
    "h"  'evil-window-left
    "k"  'kill-buffer
    "l"  'evil-window-right
    "o"  'delete-other-windows
    "r"  'revert-buffer
    "t"  'custom/gptel-send
    "w"  'save-buffer
    "x"  'execute-extended-command
    ))

(evil-mode t)
(custom/config-evil)

(straight-use-package 'evil-collection)
(require 'evil-collection)
(evil-collection-init (remove '(term term ansi-term multi-term) evil-collection-mode-list))
(setq evil-collection-setup-minibuffer t)

(straight-use-package 'evil-leader)
(require 'evil-leader)
(global-evil-leader-mode)
(custom/config-evil-leader)
(with-current-buffer "*Messages*" (evil-leader-mode))

(straight-use-package 'evil-surround)
(require 'evil-surround)
(global-evil-surround-mode)

(provide 'evil-init)
