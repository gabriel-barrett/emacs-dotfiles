;; -*- lexical-binding: t -*-
(require 'cl-lib)

;; Increase garbage collector threshold. Inspect `gcs-done'.
(setq gc-cons-threshold (* 16 1024 1024))

;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun custom/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook 'custom/reset-file-name-handler-alist)

;; Inhibit resize
(setq frame-inhibit-implied-resize t)

;; Load newer source as opposed to older bytecode
(setq load-prefer-newer t)

;; Fullscreen
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; Do not load package.el since we are gonna use either straight or guix
(setq package-enable-at-startup nil)
