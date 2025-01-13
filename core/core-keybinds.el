;;; core-keybinds.el --- core module that sets up the basic configuration -*- lexical-binding: t -*-
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
;;
;; Contains keybinds configuration.
;;
;;; Code:

(require 'core-straight)

(defvar dark-leader-map (make-sparse-keymap) "key-map for leader key")
(defvar dark-buffer-map (make-sparse-keymap) "key-map for buffer commands")
(defvar dark-window-map (make-sparse-keymap) "key-map for window commands")
(defvar dark-file-map (make-sparse-keymap) "key-map for file commands")
(defvar dark-toggle-map (make-sparse-keymap) "key-map for toggle commands")
(defvar dark-open-map (make-sparse-keymap) "key-map for open commands")
(defvar dark-version-control-map (make-sparse-keymap) "key-map for version control commands")


(define-key dark-leader-map (kbd "b") (cons "buffer" dark-buffer-map))
(define-key dark-leader-map (kbd "f") (cons "file" dark-file-map))
(define-key dark-leader-map (kbd "o") (cons "open" dark-open-map))
(define-key dark-leader-map (kbd "t") (cons "toggle" dark-toggle-map))
(define-key dark-leader-map (kbd "v") (cons "version-control" dark-version-control-map))
(define-key dark-leader-map (kbd "w") (cons "window" dark-window-map))

(define-key dark-leader-map (kbd "g") (cons "goto" goto-map))
(define-key dark-leader-map (kbd "h") (cons "help" help-map))
(define-key dark-leader-map (kbd "s") (cons "search" search-map))

;; Remove binding to view-echo-area-messages when clicking on inactive minibuffer
;;(define-key minibuffer-inactive-mode-map (kbd "<mouse-1>") nil)

;; remove keybind for suspend-frame
;;(global-unset-key (kbd "C-z"))

;; Don't kill windows when clicking on the mode line
(global-unset-key [mode-line mouse-2])
(global-unset-key [mode-line mouse-3])


;; Here we define some helper variables and functions to mimic the bahaviour
;; of =meow-mode-state-list= for minor modess
(defvar dark-meow-desired-state nil
  "Buffer-local variable to specify the desired Meow state.")

(defun dark-meow-set-desired-state (state)
  "Set the buffer-local variable =dark-meow-desired-state= to the specified state."
  (setq-local dark-meow-desired-state state))

(defun dark-meow-mode-get-state-advice (orig-func &rest args)
  "Advice function to modify =meow--mode-get-state= based on =dark-meow-desired-state=."
  (if dark-meow-desired-state
      dark-meow-desired-state
    (apply orig-func args)))

(defun dark-meow-git-timemachine-hook ()
  "Hook to set dark-meow-desired-state to =motion= when entering git-timemachine mode."
  (dark-meow-set-desired-state 'motion))

(defun dark-tab-line-mode-hook ()
  "modify behavior of meow commands when tab-line-mode is active"
  (if tab-line-mode
      (advice-add #'meow-quit :override #'dark-tab-line-close-tab-function)
    (advice-remove #'meow-quit #'dark-tab-line-close-tab-function)))


(package! meow
  :hook ((git-timemachine-mode . dark-meow-git-timemachine-hook)
         (after-init . meow-global-mode)
         (tab-line-mode . dark-tab-line-mode-hook))
  :demand t
  :custom
  ;; use system clipboard
  (meow-use-clipboard t)
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :config
  ;; Apply advice to 'meow--mode-get-state'
  (advice-add 'meow--mode-get-state :around #'dark-meow-mode-get-state-advice)
  (define-key meow-normal-state-keymap (kbd "SPC") dark-leader-map)
  (define-key meow-motion-state-keymap (kbd "SPC") dark-leader-map)
  :bind
  (("<escape>" . keyboard-escape-quit)
   ("C-<backspace>" . dark-backward-kill-thing)
   :map dark-buffer-map
   ("e" . eval-buffer)
   ("k" . kill-this-buffer)
   ("K" . kill-buffer)
   ("c" . clone-buffer)
   ("r" . revert-buffer)
   ("e" . eval-buffer)
   ("s" . save-buffer)
   :map dark-file-map
   ("f" . find-file)
   ("F" . find-file-other-window)
   ("d" . find-dired)
   ("c" . copy-file)
   ("f" . find-file)
   ("d" . delete-file)
   ("r" . rename-file)
   ("w" . write-file)
   :map dark-open-map
   ("F" . make-frame)
   ("i" . ielm)
   ("e" . eshell)
   ("t" . term)
   ("s" . scratch-buffer)
   :map dark-toggle-map
   ("M" . dark-toggle-minimal-ui)
   :repeat-map dark-window-map
   ("n" . next-window-any-frame)
   ("p" . previous-window-any-frame)
   ("k" . delete-window)
   ("K" . kill-buffer-and-window)
   ("+" . enlarge-window)
   ("-" . shrink-window)
   ("*" . enlarge-window-horizontally)
   ("’" . shrink-window-horizontally)
   ("r" . split-window-right)
   ("b" . split-window-below)
   ("v" . split-window-vertically)
   ("h" . split-window-horizontally)
   ("m" . delete-other-windows)
   ("m" . delete-other-windows)
   ("M" . delete-other-windows-vertically)
   :exit
   ("=" . balance-windows)

   :map meow-motion-state-keymap
   ("<escape>" . meow-cancel-selection)
   ("," . meow-inner-of-thing)
   ("." . meow-bounds-of-thing)
   ("b" . meow-back-word)
   ("e" . meow-next-word)
   ("f" . meow-find)
   ("o" . meow-block)
   ("q" . meow-quit)
   ("t" . meow-till)
   ("v" . meow-visit)
   ("w" . meow-mark-word)
   ("x" . meow-line)
   ("y" . meow-save)
   ("E" . meow-next-symbol)
   ("W" . meow-mark-symbol)
   ("X" . meow-goto-line)
   :map dark-leader-map
   ("?" . meow-cheatsheet)
   :map meow-normal-state-keymap
   ("'" . repeat)
   ("," . meow-inner-of-thing)
   ("-" . negative-argument)
   ("." . meow-bounds-of-thing)
   ("0" . meow-expand-0)
   ("1" . meow-expand-1)
   ("2" . meow-expand-2)
   ("3" . meow-expand-3)
   ("4" . meow-expand-4)
   ("5" . meow-expand-5)
   ("6" . meow-expand-6)
   ("7" . meow-expand-7)
   ("8" . meow-expand-8)
   ("9" . meow-expand-9)
   (";" . meow-reverse)
   ("<escape>" . meow-cancel-selection)
   ("=" . meow-indent)
   ("A" . meow-open-below)
   ("B" . meow-back-symbol)
   ("C" . meow-comment)
   ("D" . meow-backward-delete)
   ("E" . meow-next-symbol)
   ("G" . meow-grab)
   ("H" . meow-left-expand)
   ("I" . meow-open-above)
   ("J" . meow-next-expand)
   ("K" . meow-prev-expand)
   ("L" . meow-right-expand)
   ("O" . meow-to-block)
   ("Q" . meow-goto-line)
   ("R" . undo-redo)
   ("U" . meow-undo-in-selection)
   ("W" . meow-mark-symbol)
   ("X" . meow-goto-line)
   ("Y" . meow-sync-grab)
   ("[" . meow-beginning-of-thing)
   ("]" . meow-end-of-thing)
   ("a" . meow-append)
   ("b" . meow-back-word)
   ("c" . meow-change)
   ("d" . meow-delete)
   ("e" . meow-next-word)
   ("f" . meow-find)
   ("h" . meow-left)
   ("i" . meow-insert)
   ("j" . meow-next)
   ("k" . meow-prev)
   ("l" . meow-right)
   ("m" . meow-join)
   ("n" . meow-search)
   ("o" . meow-block)
   ("p" . meow-yank)
   ("q" . meow-quit)
   ("r" . meow-replace)
   ("s" . meow-kill)
   ("t" . meow-till)
   ("u" . meow-undo)
   ("v" . meow-visit)
   ("w" . meow-mark-word)
   ("x" . meow-line)
   ("y" . meow-save)
   ("z" . meow-pop-selection)))


(provide 'core-keybinds)
;;; core-keybinds.el ends here
