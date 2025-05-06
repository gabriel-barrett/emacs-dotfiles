;; -*- lexical-binding: t -*-

(defun custom/display-startup-time ()
  "Measure the init time and display it at startup"
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'custom/display-startup-time)

;; Load custom file
(load custom-file 'noerror)
