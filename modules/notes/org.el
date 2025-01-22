;;; org.el --- configuration de Org -*- lexical-binding: t; -*-
;; Copyright (C) 2025 DarkBuffalo
;; Author: DarkBuffalo (rot13 "qnexohssnyb@tah.er")
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
(require 'core-module)
(require 'core-keybinds)

(package! org
  :init
  (setq org-directory +dark-roam-dir
        org-attach-directory (concat +dark-data-dir "attachments")
        org-cite-global-bibliography (file-expand-wildcards (expand-file-name "bib/*.bib" org-directory)))
  :custom
  (org-use-sub-superscripts nil) ;; pas de sub ou supscripts
  (org-ellipsis " [+]")
  (org-src-fontify-natively t)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 2)
  (org-hide-block-startup nil)
  (org-src-preserve-indentation nil)
  ;; Return or left-click with mouse follows link
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  ;; Display links as the description provided
  (org-link-descriptive t)
  (org-startup-indented t)
  (org-startup-folded 'fold)

  ;; Todo
  (org-todo-keywords
   '((sequence
      "PROJ(p)"  ; A project, which usually contains other tasks
      "TODO(t)"  ; A task that needs doing & is ready to do
      "NEXT(n)"  ; Next task in a project
      "STRT(s)"  ; A task that is in progress
      "WAIT(w)"  ; Something external is holding up this task
      "MAYB(m)"  ; This task is paused/on hold because of me
      "DLGT"
      "|"
      "DONE(d)"  ; Task successfully completed
      "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
     (sequence  "BILLING(b)" "INVOICED(i)" "|" "PAID(P)")
     (sequence
      "[ ](T)"   ; A task that needs doing
      "[-](P)"   ; Task is in progress
      "[?](W)"   ; Task is being held up or paused
      "|"
      "[X](D)"))) ; Task was completed
  (org-todo-keyword-faces
   '(("[-]"  . +org-todo-active)
     ("STRT" . +org-todo-active)
     ("[?]"  . +org-todo-onhold)
     ("WAIT" . +org-todo-onhold)
     ("MAYB" . +org-todo-onhold)
     ("PROJ" . +org-todo-project)))

  ;; Fichiers agenda
  (org-agenda-files
   (mapcar 'file-truename
           (directory-files-recursively +dark-project-dir "\\.org$")))



  ;; Refile and Archive
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets `((,(expand-file-name  "agenda.org" +dark-project-dir) :maxlevel . 3)
                        (,(expand-file-name  "projects.org" +dark-project-dir) :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))


  (org-agenda-custom-commands
   '(("g" "Get Things Done (GTD)"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-agenda-start-day "today")
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'deadline))
                (org-deadline-warning-days 0)))
       (todo "PROJ"
             ((org-agenda-skip-function
               '(org-agenda-skip-subtree-if 'nottodo '("TODO" "NEXT" "STRT")))
              (org-agenda-overriding-header "Active Projects:")))
       (todo "TODO"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-sorting-strategy '(priority-down category-keep effort-up))
              (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nActive Tasks\n")
              ))  ; Exclude entries with LITERATURE category
       (todo "NEXT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-sorting-strategy '(priority-down category-keep effort-up))
              (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nNext Tasks\n")))
       (agenda nil
               ((org-agenda-entry-types '(:deadline))
                (org-agenda-format-date "")
                (org-deadline-warning-days 7)
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                (org-agenda-overriding-header "\nDeadlines")))
       (tags-todo "inbox"
                  ((org-agenda-prefix-format "  %?-12t% s")
                   (org-agenda-overriding-header "\nInbox\n")))
       (todo "HOLD|WAIT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-sorting-strategy '(priority-down category-keep effort-up))
              (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nTaches en pause\n")))
       (tags "CLOSED>=\"<today>\""
             ((org-agenda-overriding-header "\nRéalisés Aujourd'hui\n"))))
      ((org-agenda-category-filter-preset '("-LITERATURE"))))

     ("l" "LDC"
      ((agenda ""
               ((org-agenda-overriding-header "Calendrier LDC:")
                (org-agenda-show-log t)
                (org-agenda-log-mode-items '(clock state))
                ;;(org-agenda-category-filter-preset '("-Habitudes"))
                (org-agenda-span 5)
                (org-agenda-remove-tags nil)
                (org-agenda-start-on-weekday t)
                (org-agenda-files (directory-files-recursively +dark-work-dir "\\.org$"))
                (org-deadline-warning-days 30)))
       (tags-todo "+achat"
                  ((org-agenda-overriding-header "Achats")
                   (org-tags-match-list-sublevels nil)))
       (tags-todo "CATEGORY=\"ldc\"-achat"
                  ((org-agenda-overriding-header "LDC")))
       (tags-todo "CATEGORY=\"chateaubriant\"-achat"
                  ((org-agenda-overriding-header "Chateaubriant")))))

     ("A" "Agenda"
      ((agenda "" (;;(org-agenda-start-day "+0d")
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-span 7)
                   (org-agenda-show-log t)
                   (org-agenda-skip-scheduled-if-done t)
                   (org-agenda-skip-deadline-if-done t)
                   (org-agenda-overriding-header (concat (nerd-icons-mdicon "nf-md-calendar" :height 1.2) " Aujourd' hui:"))
                   (org-agenda-repeating-timestamp-show-all nil)
                   (org-agenda-remove-tags t)
                   ;;(org-agenda-todo-keyword-format (all-the-icons-material "check_box_outline_blank" :height 1.2))
                   (org-agenda-current-time-string "⮜┈┈┈┈┈┈ maintenant")
                   ;;(org-agenda-scheduled-leaders '("" ""))
                   (org-agenda-time-grid (quote ((daily today require-timed remove-match)
                                                 (0600 0800 1000 1200 1400 1600 1800 2000 2200)
                                                 "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈")))))
       (todo "TODO" ((org-agenda-overriding-header (concat (nerd-icons-faicon "nf-fa-check_square" :v-adjust 0.01) " A FAIRE"))
                     (org-agenda-remove-tags t)))
       (todo "TODO|PROJ"
             ((org-agenda-overriding-header (concat (nerd-icons-faicon "nf-fa-black_tie" :v-adjust 0.01 :height 1.2) " WORK"))
              (org-agenda-max-entries 10)
              (org-agenda-files (directory-files-recursively +de-work-dir "\\.org$"))))
       (todo "BILLING"
             ((org-agenda-overriding-header (concat (nerd-icons-faicon "nf-fa-usd" :v-adjust 0.01) " A FACTURER"))
              (org-agenda-max-entries 10)
              (org-agenda-files (directory-files-recursively +de-work-dir "\\.org$"))))
       (todo "NOTE"
             ((org-agenda-overriding-header (concat (nerd-icons-faicon "nf-fa-sticky_note" :v-adjust 0.01) " Notes"))
              (org-agenda-max-entries 10)
              (org-agenda-sorting-strategy
               '(tsia-down))))))

     ("G" "Get Things Done (GTD)"
      ((agenda ""
               ((org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'deadline))
                (org-deadline-warning-days 0)))
       (todo "DOING"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nTaches en cours\n")))
       (agenda nil
               ((org-agenda-entry-types '(:deadline))
                (org-agenda-format-date "")
                (org-deadline-warning-days 7)
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'notregexp "\\* DOING"))
                (org-agenda-overriding-header "\nDeadlines 7 days")))
       (todo "CLARIFY"
             ((org-agenda-prefix-format "  %?-12t% s")
              (org-agenda-overriding-header "\nNext tasks\n")))
       (todo "TODO"
             ((org-agenda-prefix-format "  %?-12t% s")
              (org-agenda-overriding-header "\nInbox\n")))
       (tags "CLOSED>=\"<today> - 7d\""
             ((org-agenda-overriding-header "\nAccomplis les 7 derniers jours\n")))))

     ))

  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)
                              (css . t)
                              (dot . t)
                              (org . t)
                              (plantuml . t)
                              (gnuplot . t)
                              (sqlite . t)
                              (shell . t)))
  (org-export-backends '(md beamer odt latex icalendar html ascii))
  (org-cite-biblatex-options "hyperref=true,url=true,backend=biber,natbib=true")

  ;; Use SVGs for latex previews -> No blur when scaling
  (org-preview-latex-default-process 'dvisvgm)

  :preface
  (defun +org-insert-file-link ()
    "Insert a file link.  At the prompt, enter the filename."
    (interactive)
    (org-insert-link nil (org-link-complete-file)))

  ;; https://github.com/rougier/emacs-gtd#activating-tasks
  (defun dark-log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  ;; Save the corresponding buffers
  (defun dark-gtd-save-org-buffers ()
    "Save `org-agenda-files' buffers without user confirmation.
                          See also `org-save-all-org-buffers'"
    (interactive)
    (message "Saving org-agenda-files buffers...")
    (save-some-buffers t (lambda ()
                           (when (member (buffer-file-name) org-agenda-files)
                             t)))
    (message "Saving org-agenda-files buffers... done"))

  ;; archive all DONE tasks in subtree
  ;; https://stackoverflow.com/questions/6997387
  (defun dark-org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'tree))

  :hook
  (org-after-todo-state-change . dark-log-todo-next-creation-date)
  :bind
  (:map dark-leader-map
        ("c" . org-capture)
        :map dark-open-map
        ("a" . org-agenda)
        :map org-mode-map
        ("C-c i" . +dark-org-inclure-fichier-ou-piece-jointe))
  :config
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (dark-gtd-save-org-buffers)))

  (setq org-log-into-drawer t
        org-log-done 'note
        org-log-reschedule 'note
        org-log-redeadline 'note
        org-log-delschedule 'note
        org-log-deldeadline 'note
        ;; Setup log note templates. Add "to [new date]" in reschedule and redeadline
        org-log-note-headings '((done        . "Note de fin %t")
                                (state       . "State %-12s from %-12S %t")
                                (note        . "Note prise le %t")
                                (reschedule  . "Schedule changed on %t: %S -> %s")
                                (delschedule . "Not scheduled, was %S on %t")
                                (redeadline  . "Deadline changé le %t: %S -> %s")
                                (deldeadline . "Supression deadline le %S a %t")
                                (refile      . "Démenagé le %t")
                                (clock-out . "")))

  (add-to-list 'org-structure-template-alist '("se" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("ss" . "src sh"))
  (add-to-list 'org-structure-template-alist '("sp" . "src python"))
  );; end org



(package! org-modern
  :custom
  (org-modern-fold-stars '(("▶" . "▼") ("▹" . "▿") ("▸" . "▾")))
  (org-modern-star 'fold)
  (org-modern-label-border 0.3)

  ;; Edit settings
  (org-auto-align-tags t)
  (org-tags-column 75)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)

  ;; Agenda styling
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))


(package! org-modern-indent
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :hook
  (org-indent-mode . org-modern-indent-mode))


(package! org-noter
  :after org
  :custom
  ;; The WM can handle splits
  ;; org-noter-notes-window-location 'other-frame
  ;; Please stop opening frames
  (org-noter-always-create-frame nil)
  ;; I want to see the whole file
  (org-noter-hide-other nil)
  ;; Everything is relative to the main notes file
  ;; org-noter-notes-search-path (list bibtex-completion-notes-path)
  (org-noter-highlight-selected-text t)
  (org-noter-auto-save-last-location t)
  (org-noter-separate-notes-from-heading t)
  (org-noter-notes-search-path '(+dark-notes-dir))
  :hook
  ;; Org-noter’s purpose is to let you create notes that are kept in sync when
  ;; you scroll through the [PDF etc] document
  (org-noter-insert-heading . org-id-get-create))


(package! ox-pandoc
  :if (executable-find "pandoc")
  :after ox
  :demand t
  :custom
  (org-pandoc-options
   '((standalone . t)
     (mathjax . t)
     (variable . "revealjs-url=https://revealjs.com")))
  :config
  (add-to-list 'org-export-backends 'pandoc))


(package! ox-tufte)

;; Generation de compte rendu
(package! ox-report)


;; reveal
(package! revealjs
  :straight (:host github :repo "hakimel/reveal.js"
                   :files ("css" "dist" "js" "plugin")))

(package! org-re-reveal
    :after ox
    :custom
    (setq org-re-reveal-root (concat "file://" (expand-file-name "../js/reveal.js" (locate-library "reveal.js")))
          org-re-reveal-theme "white"
          org-re-reveal-transition "slide"
          org-re-reveal-plugins '(markdown notes math search zoom)
          org-re-reveal-revealjs-version "4"))

(package! org-re-reveal-citeproc)
