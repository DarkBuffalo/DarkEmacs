(require 'core-straight)

(package! geiser)

(package! racket-mode
  :after (geiser smartparens org)
  :if (executable-find "racket")
  :mode ("\\.rkt[dl]?\\'" . racket-mode)
  :interpreter "racket"
  :init
  (setq geiser-scheme-implementation 'racket
        racket-smart-open-bracket-enable t)
  :config
  (add-to-list 'org-babel-load-languages '(racket . t))
  (require 'racket-xp)
  :hook (racket-mode . racket-xp-mode))

(package! geiser-racket)
