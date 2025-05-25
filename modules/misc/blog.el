

  (package! ox-hugo
    :after ox)

  (package! easy-hugo
    :init
    (setq easy-hugo-org-header t ;; utiliser les entetes org
          easy-hugo-previewtime "300")
    :config
    (easy-hugo-enable-menu))
