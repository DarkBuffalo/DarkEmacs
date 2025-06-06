;;; iconography.el --- Emacs configuration file  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 DarkBuffalo
;; Author: DarkBuffalo
;; Version: 20250111.2322
;; Keywords: configuration
;; Homepage: https://github.com/DarkBuffalo/emacs.d/
;;
;;           ______              __    _______        ___  ___       __
;;          |   _  \ .---.-.----|  |--|   _   .--.--.'  _.'  _.---.-|  .-----.
;; ^_/-...-\_^  |   \|  _  |   _|    <|.  1   |  |  |   _|   _|  _  |  |  _  |
;; \__/> o\__/  |    |___._|__| |__|__|.  _   |_____|__| |__| |___._|__|_____|
;;    \   / |:  1    /                |:  1    \
;;    (^_^) |::.. . /                 |::.. .  /
;;          `------'                  `-------'
;;
;; This file is not part of GNU Emacs.

;;; Commentary: Fichiers de configurations
;;
;;
;;; Code:

(require 'core-straight)

(package! nerd-icons
  :commands (nerd-icons-octicon
             nerd-icons-faicon
             nerd-icons-flicon
             nerd-icons-wicon
             nerd-icons-mdicon
             nerd-icons-codicon
             nerd-icons-devicon
             nerd-icons-ipsicon
             nerd-icons-pomicon
             nerd-icons-powerline)
  :config
  (do-once-n-sec-after-emacs-startup!
   0.1
   (when (not (member "Symbols Nerd Font Mono" (font-family-list)))
     (nerd-icons-install-fonts t)
     (revert-buffer))))



;; Integrate `nerd-icons' with `ibuffer'
(package! nerd-icons-ibuffer
  :defer t
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Integrate `nerd-icons' with `archive-mode', `tar-mode', `dired-mode', and `ztree'
(package! nerd-icons-multimodal
  :defer t
  :straight (:host github :repo "abougouffa/nerd-icons-multimodal")
  :hook ((archive-mode tar-mode dired-mode ztree-mode) . nerd-icons-multimodal-mode))


;; Extra colors for `Info-mode'
(package! info-colors
  :hook (Info-selection . info-colors-fontify-node))
