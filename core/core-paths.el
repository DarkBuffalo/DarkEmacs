;;; core-paths.el --- some path logic -*- lexical-binding: t; -*-

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
;;; Commentary:
;;; Code:

(require 'find-lisp)

(defconst shan-interactive-p (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

;; Deprecated :: definis dans core/core
;;(defconst custom-file (concat user-emacs-directory "custom.el"))

(defvar dark-core-dir (expand-file-name "core" user-emacs-directory)
  "Directory with all the core files.")

(defvar dark-modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory with all the module files.")

(defvar +dark-roam-dir (expand-file-name "~/Org/"))
(defvar +dark-data-dir (concat +dark-roam-dir ".data/"))
(defvar +dark-project-dir (concat +dark-roam-dir "projects/"))

(defconst dark-local-dir (expand-file-name ".local/" user-emacs-directory)
  "Root directory for local storage.
TODO: figure out how to move to doom esque management from `no-littering'.")

(defconst dark-assets-dir (expand-file-name "assets/" user-emacs-directory)
  "Directory with all the module files.")

(defconst dark-etc-dir (expand-file-name "etc/" dark-local-dir)
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst dark-var-dir (expand-file-name "var/" dark-local-dir)
  "Directory for installed and created files for config.")

(defconst dark-cache-dir (expand-file-name "cache/" dark-var-dir)
  "Directory for volatile local storage.
Use this for files that change often, like cache files.")

(provide 'core-paths)
;;; core-paths.el ends here
