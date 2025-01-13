(require 'core-straight)

(dolist (fn '(line-number-mode column-number-mode))
  (if (fboundp fn)
      (funcall fn t)))

(package! doom-modeline
  :demand t
  :config
  (setq doom-modeline-support-imenu t
        doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-buffer-encoding t
        doom-modeline-time t
        doom-modeline-battery t
        doom-modeline-hud nil
        doom-modeline-window-width-limit 120
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'file-name


        doom-modeline-minor-modes nil
        doom-modeline-env-version t
        doom-modeline-python-executable "python3")
  (doom-modeline-mode t))

(package! hide-mode-line
  :hook
  ((neotree-mode
    imenu-list-minor-mode
    minimap-mode ibuffer-mode
    help-mode
    deft-text-mode
    Man-mode)
   . hide-mode-line-mode))


(with-feature! +keycast
  (package! keycast
    :commands (+toggle-keycast)
    :config
    (defun +toggle-keycast()
      (interactive)
      (if (member '("" keycast-mode-line " ") global-mode-string)
          (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
                 (remove-hook 'pre-command-hook 'keycast--update)
                 (message "Keycast OFF"))
        (add-to-list 'global-mode-string '("" keycast-mode-line " "))
        (add-hook 'pre-command-hook 'keycast--update t)
        (message "Keycast ON")))
    :hook (after-init . +toggle-keycast)))
