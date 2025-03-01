;; -*- lexical-binding: t -*-

(straight-use-package 'gptel)
(require 'gptel)

(setq gptel-api-key
      (lambda ()
        (custom/shell-command-to-string-on-success "gpg -d $HOME/keys/deepseek.gpg" t)))

(setq gptel-model 'deepseek-chat
      gptel-backend
      (gptel-make-openai "DeepSeek"
        :host "api.deepseek.com"
        :endpoint "/chat/completions"
        :stream t
        :key 'gptel-api-key
        :models '(deepseek-chat deepseek-coder)))

(setq gptel-temperature 0.0)

(provide 'ai-init)
