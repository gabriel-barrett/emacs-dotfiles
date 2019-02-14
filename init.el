;; Packages
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; No startup screen
(setq inhibit-startup-screen 1)

;; Basic settings for backup
(setq
   backup-by-copying 1      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions 1
   kept-new-versions 6
   kept-old-versions 2
   version-control 1)       ; use versioned backups

;; Theme
(load-theme 'zenburn 1)

;; Misc
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ido and Smex (if not using helm)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(setq ido-auto-merge-work-directories-length -1)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Display lines and columns
(setq column-number-mode 1)

;; Evil mode
(setq evil-want-C-u-scroll 1)
(require 'evil)
(evil-mode 1)

;; Org, LaTeX, Markdown and Pandoc
(require 'latex-pretty-symbols)
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'markdown-mode-hook 'latex-unicode-simplified)
(setq markdown-enable-math 1)
(add-hook 'org-mode-hook 'latex-unicode-simplified)

;; Linum Relative
(require 'linum-relative)
(linum-relative-global-mode 1)
(setq linum-relative-current-symbol "")
(set-face-attribute 'linum nil :height 110)

;; Open .v files with Proof General's Coq mode
(load "~/.emacs.d/lisp/PG/generic/proof-site")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-latex-classes
   (quote
    (("article" "\\documentclass[11pt]{article}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(package-selected-packages
   (quote
    (evil-leader ess ido-completing-read+ evil-magit evil-org evil-tutor polymode magit fstar-mode idris-mode redprl fsharp-mode haskell-mode rust-mode sml-mode tuareg zenburn-theme linum-relative pandoc-mode markdown-mode latex-pretty-symbols smex evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
