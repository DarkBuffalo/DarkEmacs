;;; activate.el --- -*- lexical-binding: t; -*-
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

(defvar dark--active-modules)

;;; Order slightly matters, maybe auto-ide should be part of core?
(setq dark--active-modules
      '(:tools
        auto-ide
        (lsp +dap +ui)
        (vc +git +gutter)
        ;; copilot
        (time +vundo)
        draw                            ;openscad
        ai
        web

        :input
        ;;macos

        :notes
        (org)
        notdeft
        ;; denote
        latex
        research

        :checkers
        ;;(spell +hunspell)
        (syntax
         +flycheck)
        ;;(grammar +lsp) ; requires node 16 and pnpm -g add @emacs-grammarly/grammarly-languageserver

        :completion
        (company +childframe)
        (vertico +icons)
        snippets
        corfu
        (cape +icons)

        :editor
        format
        ;; hungry-delete ; FIXME: breaks ivy and other tools
        multiple-cursors
        (parentheses +rainbow)
        (zoom +text +window)
        neotree

        :ui
        (color +todo +whitespace +nums +tokens)
        iconography
        (theme)
        (modeline +keycast)
        dashboard ;;enlight
        (discoverability)
        treemacs

        :lang
        elisp
        (lisp +slime)
        racket
        ahk
        ;; (asm +mips)
        (go +lsp +dap)
        (js +ts +jsx +tsx +vue +lsp +dap)
        (web +emmet +vtl +lsp +dap)
        (python +lsp +dap)
        shell
        ;;(yaml +lsp) ; pnpm -g add yaml-language-server
        json
        (graphql +lsp)
        tool

        :misc
        cipher
        (key-logger
         +freq +commands)
        ;; sicp
        music ;; bongo
        rss
        blog
        social
        ))

(provide 'activate)
;;; activate.el ends here
