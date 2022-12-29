;; -*- lexical-binding: t -*-
(require 'popper)

(defun popper-close-latest--bury (old-function)
  "Advice to bury popup buffers"
  (unless popper-mode (user-error "Popper-mode not active!"))
  (if (null popper-open-popup-alist)
      (message "No open popups!")
    (cl-destructuring-bind ((win . buf) . rest) popper-open-popup-alist
      (let ((group (when popper-group-function
		     (with-current-buffer buf
		       (funcall popper-group-function)))))
	(unless (cl-member buf
			   (cdr (assoc group popper-buried-popup-alist))
			   :key 'cdr)
	  ;; buffer doesn't already exist in the buried popup list
	  (push (cons nil buf) (alist-get group
					  popper-buried-popup-alist
					  nil nil 'equal))))
      (pop popper-open-popup-alist)
      (with-selected-window win
	(bury-buffer buf)
	;;only close window when window has a parent or in a child frame:
	(popper--delete-popup win)))))
(advice-add 'popper-close-latest :around 'popper-close-latest--bury)

(setq popper-reference-buffers
      '("\\*Messages\\*"
	"Output\\*$"
	"\\*Async Shell Command\\*"
	;; help-mode
	compilation-mode))
;; Match eshell, shell, term and/or vterm buffers
(setq popper-reference-buffers
      (append popper-reference-buffers
	      '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
		"^\\*shell.*\\*$"  shell-mode  ;shell as a popup
		"^\\*term.*\\*$"   term-mode   ;term as a popup
		"^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
		)))
(global-set-key (kbd "C-`") 'popper-toggle-latest)
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(popper-mode +1)

(provide 'popper-init)
