(require 'core-straight)

(package! which-key
  :hook
  (meow-mode . which-key-mode)
  :custom
  (which-key-idle-delay 0.1)
  (which-key-compute-remaps t)
  (which-key-prefix-prefix "󰜄 ")
  (which-key-separator " ")
  :config
  (which-key-setup-minibuffer)

  (with-module! :tools lsp
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))
