;; -*- lexical-binding: t -*-

;; Increase garbage collector threshold. Inspect `gcs-done'.
(setq gc-cons-threshold #x4000000)
(setq gc-cons-percentage 0.25)

;; Load newer source as opposed to older bytecode
(setq load-prefer-newer t)

;; Inhibit resize
(setq frame-inhibit-implied-resize t)

;; Fullscreen
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Backup files
(let ((backup (expand-file-name "backup/" user-emacs-directory)))
  (make-directory backup t)
  (setq backup-directory-alist `(("." . ,backup))))

;; Auto-save files
(let* ((auto-saves (expand-file-name "auto-saves/" user-emacs-directory))
       (sessions (expand-file-name "sessions/" auto-saves)))
  (make-directory sessions t)
  (setq auto-save-list-file-prefix sessions
	auto-save-file-name-transforms `((".*" ,auto-saves t))))

;; Inhibit packages
(setq package-enable-at-startup nil)
