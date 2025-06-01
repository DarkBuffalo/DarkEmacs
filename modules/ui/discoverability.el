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


(package! current-window-only
  :straight (current-window-only
             :type git
             :host github
             :repo "FrostyX/current-window-only")
  :config (curent-window-only-mode))



;;; Ultra-scroll
;; https://github.com/jdtsmith/ultra-scroll
(package! ultra-scroll
  :straight (ultra-scroll
             :host github
             :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101       ; As instructed by the README
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; DIRED
(package! dired-git-info
  :hook ('dired-after-readin 'dired-git-info-auto-enable))


(setq display-buffer-alist
      `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.33)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))


(package! highlight-indent-guides
  :defer t
    :hook (prog-mode . highlight-indent-guides-mode)
    :init
    ;; (setq highlight-indent-guides-method 'column)
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-character ?|)
    ;; (setq highlight-indent-guides-character ?❚)
    ;;(setq highlight-indent-guides-character ?‖)
    ;; (setq highlight-indent-guides-responsive 'stack)
    (setq highlight-indent-guides-responsive 'top)
    ;; (setq highlight-indent-guides-auto-enabled nil)
    ;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
    ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
    ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
    (setq highlight-indent-guides-suppress-auto-error t))
