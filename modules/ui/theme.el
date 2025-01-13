(require 'core-straight)

(setq frame-background-mode 'dark)

;; Doom themes are best esp since I use Doom modeline. Also, Doom Dracula just
;; has better keyword support as far as I’ve seen. It’s also just easy on my
;; eyes.
(package! doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;; The dark nights sometimes need a little sun. The slight brightness is nice
;; for the eyes.
(package! solaire-mode
  :if (feature-p! +solaire)
  :when (or (daemonp) (display-graphic-p))
  :demand t
  :functions persp-load-state-from-file
  :hook
  (minibuffer-setup . turn-off-solaire-mode)
  :config
  (setq solaire-mode-remap-modeline nil
        solaire-mode-remap-fringe nil)
  (solaire-global-mode 1)
  (advice-add #'persp-load-state-from-file
              :after #'solaire-mode-restore-persp-mode-buffers))

;; ui a la vi dans les buffers
(package! vi-tilde-fringe
  :hook ((prog-mode . vi-tilde-fringe-mode)
         (org-mode . vi-tilde-fringe-mode)))

(package! hide-mode-line
  :hook
  (((enlight-mode org-agenda-mode completion-list-mode man-mode symbols-outline-mode shell-mode pdf-view-mode) . hide-mode-line-mode)))
