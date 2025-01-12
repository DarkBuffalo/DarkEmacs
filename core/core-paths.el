;;; core-paths.el --- some path logic -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'find-lisp)

(defconst shan-interactive-p (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

(defconst custom-file (concat user-emacs-directory "custom.el"))

(defvar dark-core-dir (expand-file-name "core" user-emacs-directory)
  "Directory with all the core files.")

(defvar dark-modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory with all the module files.")

(defconst dark-local-dir (expand-file-name ".local" user-emacs-directory)
  "Root directory for local storage.

TODO: figure out how to move to doom esque management from `no-littering'.")

(defconst dark-etc-dir (expand-file-name "etc" dark-local-dir)
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst dark-var-dir (expand-file-name "var" dark-local-dir)
  "Directory for installed and created files for config.")

(defconst shan-cache-dir (expand-file-name "cache" dark-var-dir)
  "Directory for volatile local storage.
Use this for files that change often, like cache files.")

(provide 'core-paths)
;;; core-paths.el ends here
