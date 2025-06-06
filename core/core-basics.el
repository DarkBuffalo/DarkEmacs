;;; core-basics.el --- core module that sets up the basic configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Contains basic configuration.
;;; Code:

(require 'core-straight)

;; Encoding
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(setq-default locale-coding-system 'utf-8)
(dolist (fn '(set-terminal-coding-system set-keyboard-coding-system set-selection-coding-system prefer-coding-system))
  (if (fboundp fn)
      (funcall fn 'utf-8)))

;; Encodage général en UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(modify-coding-system-alist 'process "*" 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Encoding
;; We tell emacs to use UTF-8 encoding as much as possible.


(set-language-environment "UTF-8") ; Set up multilingual environment
(setq org-export-html-coding-system 'utf-8
      htmlize-html-charset "utf-8")
(set-selection-coding-system
 (if (eq system-type 'windows-nt)
     'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
   'utf-8))

(package! unidecode)

;; Backups
;;; I don't particularly need backup files, and `~' + `#' files are a pain to clean anyways
(setq-default backup-inhibited t
              auto-save-default nil
              create-lockfiles nil
              make-backup-files nil)

;; Confirmation Messages
;;; kill processes when leaving emacs
(when (>= emacs-major-version 26)
  (setq-default confirm-kill-processes nil))

;;; Who types a whole `yes' intead of `y' nowadays..?
;;; TODO: look into other options which still require full words to be typed
(defalias 'yes-or-no-p (lambda (&rest _) t))
(setq-default confirm-kill-emacs nil)
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; Appearance
;;; you need to experience keyboard to realize keyboard master race. (`fn+f10' if need be for options though)
(setq inhibit-startup-message t)
(dolist (fn '(tool-bar-mode scroll-bar-mode menu-bar-mode))
  (if (fboundp fn)
      (funcall fn -1)))

;;; prevents some cases of flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


(custom-set-faces
 '(default
   ((((type ns)) ;; mac-specific config
     ( :family "Iosevka NF"
       :height 180))
    (t
     (:family "Iosevka NF"
      :height 120)))))


;;; Noto just seems to break emacs(?)
(add-to-list 'face-ignored-fonts "Noto Color Emoji")

;;; Symbola for emoji!
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Interface
;;; okay, these bells are annoying... they're not even useful half the time.
(setq-default visible-bell nil
              audible-bell nil
              ring-bell-function 'ignore)

;;; Lines, need to look into these later for focus
(setq-default transient-mark-mode t
              visual-line-mode t
              indent-tabs-mode nil
              tab-width 4)

;;; line numbers are pretty slow, line numbers in the modeline should be good enough
(setq display-line-numbers-type nil)

;;; highlights the line containing mark
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t))

;;; TODO: optimize the cursor even more...
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode 0))

(setq-default blink-matching-paren nil
              visible-cursor nil
              x-stretch-cursor nil
              cursor-type 'box)

;; Scratch Buffer
(setq-default initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message nil)

;; Actual Core Basics -- Default Sane Config
(setq-default require-final-newline t
              fill-column 80
              vc-follow-symlinks t
              find-file-visit-truename t
              inhibit-compacting-font-caches t
              use-short-answers t ; Replace yes/no prompts with y/n
              native-comp-async-report-warnings-errors 'silent ; disable native compiler warnings
              indicate-buffer-boundaries 'left ; equer dans les angles
              )

(global-subword-mode t)
(delete-selection-mode t)
(global-font-lock-mode t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; (global-set-key (kbd "M-;")
;;                 'comment-line)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)

;;; how do people live without smart expansion?
(package! expand-region
  :bind
  ("C-=" . er/expand-region))

(defun dark/fill-or-unfill ()
  "Fill or unfill based on the previous command."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'dark/fill-or-unfill)

;;; TODO: look into moving larger bulks of texts
(package! move-text
  :config
  (move-text-default-bindings))

;; TODO: refine this after refining  buffers, search, and projectile
;; Recently opened files
(require 'recentf)

(setq recentf-save-file (expand-file-name "recentf.el" dark-cache-dir)
      recentf-max-saved-items 100
      recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                          "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                          ".*png$" ".*cache$"))

(defun doom--recentf-file-truename-fn (file)
  "Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
abbreviate $HOME -> ~ in filepaths (more portable, more readable, & saves
space)"
  (if (or (not (file-remote-p file))
          (equal "sudo" (file-remote-p file 'method)))
      (abbreviate-file-name (file-truename (tramp-file-name-localname tfile)))
    file))

(add-to-list 'recentf-filename-handlers #'doom--recentf-file-truename-fn)

;;; Text properties inflate the size of recentf's files, and there is
;;; no purpose in persisting them (Must be first in the list!)
(add-to-list 'recentf-filename-handlers #'substring-no-properties)

;; TODO: refine this somewhere between refining dashboard, buffers, search, and projectile
;; Persist variables across sessions
(setq savehist-file (expand-file-name "savehist.el" dark-cache-dir))



;; demo elisp
(package! elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))



(provide 'core-basics)
;;; core-basics.el ends here
