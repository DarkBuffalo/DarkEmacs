(defun dark-pdf-tools-themed-update-advice (&rest app)
  (when pdf-view-themed-minor-mode
    (pdf-view-refresh-themed-buffer t)))


(with-os! (windows-nt)
  ;; https://github.com/vedang/pdf-tools?tab=readme-ov-file#installing-the-epdfinfo-server-from-source-on-windows--gotchas
  ;; Requis: pacman -S mingw-w64-x86_64-emacs-pdf-tools-server
  (package! pdf-tools
    :straight (:host github :repo "vedang/pdf-tools")
    ;;:functions (pdf-view-refresh-themed-buffer)
    :custom
    (pdf-view-use-scaling t)
    :config
    (pdf-tools-install :no-query)
    (advice-add #'enable-theme :after #'dark-pdf-tools-themed-update-advice)
    :hook
    ((pdf-view-mode . pdf-view-themed-minor-mode)
     (pdf-view-mode . pdf-isearch-minor-mode))))
