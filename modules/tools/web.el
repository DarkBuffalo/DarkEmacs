

(package! emacs-websearch
  :straight '(emacs-websearch
              :host github
              :repo "zhenhua-wang/emacs-websearch")
  :bind (("C-c l" . emacs-websearch))
  :config
  (setq emacs-websearch-engine 'duckduckgo
        browse-url-browser-function 'browse-url-default-browser
        emacs-websearch-async t))
