;; -*- lexical-binding: t -*-
;; Measure the init time
(defun custom/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		   (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'custom/display-startup-time)

;; Custom function to add paths to Emacs
(defun custom/add-to-path-if-dir (path)
  "Add PATH to the `exec-path` and `PATH` environment variable if it is a valid directory."
  (let ((expanded-path (substitute-in-file-name path)))
    (when (file-directory-p expanded-path)
      (unless (member expanded-path exec-path)
        (setq exec-path (append exec-path (list expanded-path)))
        (setenv "PATH" (concat (getenv "PATH") path-separator expanded-path))))))

(defun custom/shell-command-to-string-on-success (command &optional ignore-stderr)
  "Call shell COMMAND and return its output as a string if the command succeeds.
Signal an error if the command exits with a non-zero status. If IGNORE-STDERR is non-nil,
discard any error output from the command."
  (let ((full-command (if ignore-stderr
                          (concat command " 2>/dev/null")
                        command)))
    (with-temp-buffer
      (let ((exit-status (call-process-shell-command full-command nil t)))
        (if (eq exit-status 0)
            (buffer-string)
          (error "Command '%s' failed with exit status %d: %s"
                 command exit-status (buffer-string)))))))

;; Custom set variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Extra config files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set mail and user name
(setq user-full-name "Gabriel Barreto")
(setq user-mail-address "gabriel.aquino.barreto@gmail.com")

;; General settings
(setq inhibit-splash-screen 1
      inhibit-startup-message 1
      inhibit-startup-echo-area-message 1
      visible-bell 1
      column-number-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Misc
(electric-pair-mode)
(show-paren-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq enable-local-variables :safe)
(setq-default indent-tabs-mode nil)

;; Transparency function
(defun custom/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Basic settings for backup, auto-saves, etc
(setq
   backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
   backup-by-copying 1
   delete-old-versions 1
   version-control 1)

(make-directory
 (expand-file-name "tmp/auto-saves/sessions" user-emacs-directory) t)
(setq
 auto-save-list-file-prefix
 (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
 auto-save-file-name-transforms
 `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq undo-tree-auto-save-history nil)

;; Relative numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Icomplete
(fido-vertical-mode 1)

;; TRAMP
(require 'tramp)
(setq tramp-remote-path
      (append tramp-remote-path
	      '(tramp-own-remote-path)))

;; Nix
(custom/add-to-path-if-dir "$HOME/.nix-profile/bin")

;; Evil mode
(setq evil-want-keybinding nil
      evil-want-integration t
      evil-respect-visual-line-mode t
      evil-want-C-u-scroll t)
(when (require 'evil nil t) (require 'evil-init))

;; Langs
(require 'langs-init)

;; Dired
(with-eval-after-load 'dired (require 'dired-init))

;; eww
(with-eval-after-load 'eww (require 'eww-init))

;; Magit
(require 'magit)

;; Term mode
(with-eval-after-load 'term
  (term-set-escape-char ?\C-x)
  (define-key term-raw-map (kbd "C-x C-y") 'term-paste))

;; Popper mode
(require 'popper-init)

;; Flycheck mode
(require 'flycheck-init)

;; Which key
(require 'which-key)
(setq which-key-idle-delay 0.2)
(which-key-mode 1)

;; Artificial intelligence
(require 'ai-init)

;; Theme
(load-theme 'spacemacs-dark 1)

;; Emacs icons

;; Doom modeline
(doom-modeline-mode t)

(provide 'init)
