;; -*- lexical-binding: t -*-
(defun evil-command-window-insert-commands--fix (old-function hist)
  "Fixes 'evil-command-window-ex empty history error"
  (let ((inhibit-modification-hooks t))
    (mapc #'(lambda (cmd) (insert cmd) (newline)) (reverse hist)))
  (let ((prefix (propertize evil-command-window-cmd-key
			    'font-lock-face 'minibuffer-prompt)))
    (set-text-properties (point-min) (point-max) (list 'line-prefix prefix)))
  (goto-char (point-max))
  (when (and (bolp) (not (bobp))) (backward-char))
  (evil-adjust-cursor))
(advice-add 'evil-command-window-insert-commands :around 'evil-command-window-insert-commands--fix)

(defun custom/config-evil ()
  "Configure evil mode."

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
    "r"  'revert-buffer
    "h"  'evil-window-left
    "l"  'evil-window-right
    "."  'mode-line-other-buffer
    "b"  'switch-to-buffer
    "c"  'comment-dwim
    "d"  'kill-this-buffer
    "D"  (lambda () (interactive) (dired "."))
    "k"  'kill-buffer
    "f"  'find-file
    "g"  'magit-status
    "o"  'delete-other-windows
    "w"  'save-buffer
    "x"  'execute-extended-command
    ))

(evil-mode t)
(custom/config-evil)

(require 'evil-collection)
(evil-collection-init (remove '(term term ansi-term multi-term) evil-collection-mode-list))
(setq evil-collection-setup-minibuffer t)

(require 'evil-leader)
(global-evil-leader-mode)
(custom/config-evil-leader)
(with-current-buffer "*Messages*" (evil-leader-mode))

(require 'evil-surround)
(global-evil-surround-mode)

(provide 'evil-init)
