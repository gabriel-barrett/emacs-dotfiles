;; -*- lexical-binding: t -*-

;; ChatGPT
(straight-use-package 'gptel)
(require 'gptel)
(setq gptel-api-key
      (lambda ()
	(custom/shell-command-to-string-on-success "gpg -d $HOME/chatgpt.api.gpg" t)))

(defun custom/gptel--create-prompt-from-string (input-string)
  "Return a full conversation prompt from INPUT-STRING.

INPUT-STRING should contain the conversation text with user and
assistant messages."
  (with-temp-buffer
    (insert input-string)
    (goto-char (point-min))
    (let ((prop) (prompts))
      (while (setq prop (text-property-search-forward
                         'gptel 'response
                         (when (get-char-property (max (point-min) (1- (point)))
                                                  'gptel)
                           t)))
        (push (list :role (if (prop-match-value prop) "assistant" "user")
                    :content
                    (string-trim
                     (buffer-substring-no-properties (prop-match-beginning prop)
                                                     (prop-match-end prop))
                     "[*# \t\n\r]+"))
              prompts))
      prompts)))

(defun custom/gptel-send ()
  "Submit a message to ChatGPT and display the output in a separate buffer."
  (interactive)
  (message "Querying ChatGPT...")
  (let* ((user-message (read-string "Enter your message: "))
         (gptel-buffer (get-buffer-create "*ChatGPT Output*"))
         (full-prompt (custom/gptel--create-prompt-from-string user-message)))
    (with-current-buffer gptel-buffer
      (if (= (point-min) (point-max)) (insert (format "Model %s" (upcase gptel-model))))
      (funcall
       (if gptel-use-curl
           #'gptel-curl-get-response #'gptel--url-get-response)
       (list :prompt full-prompt
             :buffer gptel-buffer
             :position (goto-char (point-max)))))
    (gptel--update-header-line " Waiting..." 'warning)
    (display-buffer gptel-buffer)))

(evil-leader/set-key
  "t"  'custom/gptel-send)

;; Copilot
(straight-use-package '(copilot
			:type git
			:host github
			:repo "zerolfx/copilot.el"
			:files ("dist" "*.el")))
; you can utilize :map :hook and :config to customize copilot

(provide 'ai-init)
