;; Copyright (C) 2025 DarkBuffalo
;; Author: DarkBuffalo
;; Version: 20250111.2322
;; Keywords: configuration
;; Homepage: https://github.com/DarkBuffalo/DarkEmacs/
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
;;; Commentary:
;;; Code:

(require 'core-straight)

(package! yasnippet
  :config
  ;; disable annoying messages
  ;; TODO: trigger to 4 when refining...
  (setq yas-verbosity 0)
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" dark-assets-dir))
  (yas-global-mode 1))

(package! yasnippet-snippets)

(when (< (length yas-snippet-dirs) 2)
  (require 'yasnippet)
  (yas-reload-all))
