;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-
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
;; Loads all other core modules.
;;; Code:

;; figure out proper pathing
(load (expand-file-name (expand-file-name "core/core-paths" user-emacs-directory))
      nil 'nomessage)
(require 'core-paths)
(add-to-list 'load-path dark-core-dir)

;; Actionner la gestion des paquets
(require 'core-paths)
(require 'core-straight)

;; Charger ceci pour la gestion de l'arborescence .emacs.d propre
;; load this before any other third-party packages to keep init directory clean
(package! no-littering
  :demand t
  :init
  (setq emacs-config-directory (concat dark-local-dir "config/")
        no-littering-etc-directory dark-etc-dir
        no-littering-var-directory dark-var-dir
        ;; Since init.el will be generated from this file, we save customization in a dedicated file.
        custom-file (expand-file-name "custom.el" dark-local-dir))
  (require 'no-littering))

;; en cas de bug ce paquet sauve des vies
;; extremely helpful for figuring out what went wrong with the config file
;; also, it's helpful for writing packages
(package! bug-hunter :demand t)

;; these are just cool libraries I’d like to use during my config, or many of the packages use them
(package! dash :demand t)
(package! f :demand t)
(package! s :demand t)
(package! string-inflection :demand t)

;; load core up
(dolist (core-module '(core-util
                       tricks-n-gimmicks
                       core-keybinds
                       core-module
                       core-gc
                       core-basics
                       core-projects))
  (require core-module))

(provide 'core)
;;; core.el ends here
