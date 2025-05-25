(require 'core-straight)

(defun dark-notdeft-select-note-file ()
  "Offer an choice list of all notes.
            Return a file name for the selected note. Return nil if there are
            no notes from which to select."
  (let* ((name-lst (notdeft-make-basename-list))
         (name (when name-lst
                 (completing-read "NotDeft note: " name-lst)))
         (file (when name
                 (notdeft-file-by-basename name))))
    file))

(package! notdeft
  ;; :straight (:local-repo "~/Dropbox/Git/notdeft/"
  ;;                        :files ("extras" "xapian" "*.el" ))
  :straight (:type git :host github :repo "hasu/notdeft"
                   :files ("extras" "xapian" "*.el"))
  :init
  (autoload 'notdeft-xapian-make-program-when-uncurrent "notdeft-xapian-make")
  :hook ((notdeft-load-hook . notdeft-xapian-make-program-when-uncurrent)
         ;; (org-mode-hook . notdeft-mode-hook) ;; BUG on emacs 30
         )
  :config
  (setq notdeft-extension "org")
  (setq notdeft-secondary-extensions '("md" "txt"))
  (setq notdeft-allow-org-property-drawers t)
  ;;fonction d'affichage de la liste des fichiers
  (setq notdeft-file-display-function
        (lambda (file w)
          (when (> w 30)
            (let* ((s (file-name-nondirectory
                       (directory-file-name
                        (notdeft-dir-of-file file))))
                   (s (pcase s
                        ("bibliography-notes" "bib")
                        ("homepage-notes" "hp")
                        (_ s)))
                   (s (if (> (string-width s) 12)
                          (truncate-string-to-width s 12)
                        s)))
              (concat " " s)))))
  ;; fonction pour la creation d'un nouveau fichier
  (setq notdeft-new-file-data-function
        (lambda (dir notename ext data title)
          (cons
           (notdeft-make-filename
            (or notename
                (let ((ts (format-time-string "%Y%m%d%H%M")))
                  (if title
                      (concat ts " " title)
                    ts)))
            ext dir)
           (or data
               (when title
                 (concat "#+TITLE: " title "\n\n"))))))
  (advice-add 'notdeft-ido-select-note-file :override #'dark-notdeft-select-note-file ))


(eval-after-load 'org
  (lambda ()
    (require 'notdeft-org9)))

(add-to-list 'load-path (concat user-emacs-directory ".local/straight/build/notdeft/extras"))
;; FIXME
;;(require 'notdeft-transient)
;;(global-set-key [f6] 'notdeft-transient-global-menu)
