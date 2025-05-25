
(package! ellama
  :bind ("C-c e" . ellama-transient-main-menu)
  :config
  (setq ellama-sessions-directory (expand-file-name "ellama/" dark-local-dir))
  ;; send last message in chat buffer with C-c C-c
  (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message)

  :custom
  (ellama-language                 "French")
  (ellama-spinner-type             'moon)
  (ellama-user-nick                "Matt")
  (ellama-naming-scheme            'ellama-generate-name-by-llm)
  (ellama-provider
   (make-llm-ollama
    ;; this model should be pulled to use it
    ;; value should be the same as you print in terminal during pull
    :chat-model "qwen2.5-coder:3b"
    :embedding-model "nomic-embed-text"
    :default-chat-non-standard-params '(("num_ctx" . 8192))))
  :init
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-coding-provider
          (make-llm-ollama
           :chat-model "starcoder2:3b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-summarization-provider
          (make-llm-ollama
           :chat-model "granite3.1-moe:3b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  (setopt ellama-providers
          '(("qwen2.5-coder:3b" . (make-llm-ollama
                                   :chat-model "qwen2.5-coder:3b"
                                   :embedding-model "nomic-embed-text"))
            ("llama3.2:3b" . (make-llm-ollama
                              :chat-model "llama3.2:3b"
                              :embedding-model "nomic-embed-text"))
            ("granite3.1-moe:3b" . (make-llm-ollama
                                    :chat-model "granite3.1-moe:3b"
                                    :embedding-model "nomic-embed-text"))
            ;; ("starcoder2:3b" . (make-llm-ollama
            ;;                     :chat-model "starcoder2:3b"
            ;;                     :embedding-model "nomic-embed-text"))
            )))


;;;  https://github.com/natrys/whisper.el
;; (package! whisper
;;   :straight (:type github :repo "natrywhisper/whisper.el")
;;   :bind ("C-H-r" . whisper-run)
;;   :config
;;   (setq whisper-model "base"
;;         whisper-language "fr"
;;         whisper-use-threads (/ (num-processors) 2)))


(package! elysium
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical)) ; Can be customized to horizontal
