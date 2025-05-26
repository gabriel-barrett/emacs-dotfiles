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

;; Inhibit packages
(setq package-enable-at-startup nil)
