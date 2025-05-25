(require 'core-straight)

;; Il est nécessaire sous windows d'installer [[https://gnuwin32.sourceforge.net/packages/findutils.htm][Findutils]].
;; Il est possible d'utiliser 'choco install findutils'.

(package! denote
  :custom
  ;; If you want Denote to infer keywords from your note titles, set this to t.
  (denote-infer-keywords t)
  ;; If you want to sort keywords alphabetically, set this to t.
  (denote-sort-keywords t)
  ;; Specify a regular expression to exclude directories from being searched for notes.
  (denote-excluded-directories-regexp nil)
  ;; Configure the date format used in note file names.
  (denote-date-format nil)
  ;; Disable confirmation prompts when renaming files. Use with caution!
  (denote-rename-confirmations nil)
  ;; When displaying backlinks, don't show the surrounding context.
  (denote-backlinks-show-context nil)
  ;; Configure the format used for renaming Denote buffers.
  (denote-rename-buffer-format "[D] %t%b")
  ;; String to indicate that a buffer has backlinks.
  (denote-buffer-has-backlinks-string " (<--->)")
  ;; Define templates for notes
  (denote-templates
   '((minutes . "minutes")
     (plain . nil)))
  :preface
  (defvar dark-denote-map (make-sparse-keymap) "key-map for denote commands")
  :init
  (define-key dark-leader-map (kbd "n") (cons "denote" dark-denote-map))
  :bind
  (:map global-map
        :map dark-denote-map
        ("n" . denote)
        ("N" . denote-type)
        ("r" . denote-rename-file)
        ("R" . denote-rename-file-using-front-matter)
        ("i" . denote-link)
        ("I" . denote-add-links)
        ("b" . denote-backlinks)
        ;; :map org-mode-map
        ;; ("l" . denote-org-extras-dblock-insert-links)
        ;; ("b" . denote-org-extras-dblock-insert-links)
        :map dired-mode-map
        ("i" . denote-link-dired-marked-notes)
        ("r" . denote-dired-rename-marked-files)
        ("k" . denote-dired-rename-marked-files-with-keywords)
        ("f" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (add-hook 'context-menu-functions #'denote-context-menu)
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode)
   (after-init . denote-rename-buffer-mode)))


(package! denote-menu
  :after denote
  :bind
  (:map dark-denote-map
        ("m" . list-denotes)))

(package! citar-denote
  :custom
  (citar-denote-template 'biblio)
  (citar-denote-subdir "bib_notes")
  :config
  (add-to-list 'denote-templates
               `(biblio . ,(concat
                            "#+cite_export: biblatex ieee\n"
                            (concat "#+bibliography: " (car citar-bibliography) "\n\n")
                            "* Notes :ignore:\n"
                            ":PROPERTIES:\n"
                            ":NOTER_DOCUMENT: ${file} \n"
                            ":END:\n\n"
                            "* Summary :childless:showchildren:export:\n"
                            "This is a summary of [cite/t:@${=key=}].\n"
                            "** Bibliography :ignore:\n")))
  :hook
  (after-init . citar-denote-mode))


(package! consult-denote
  :after denote consult
  :bind
  (:map dark-denote-map
        ("f" . consult-denote-find)
        ("g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))
