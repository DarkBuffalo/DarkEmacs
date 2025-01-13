(require 'core-straight)
(require 'core-util)


;; ADD QUOTE on dashboard-footer
(defvar +dark-quotes-file (expand-file-name "quotes.txt" dark-assets-dir)
  "Fichier comprenant les citations")

(defvar +dark-quotes-file-seperator-regex "\n%\n"
  "Regex de delimitation pour separer les lines dans le fichier`+dark-quotes-file'")

(defvar +dark-quotes-author-regex "^--"
  "Regex qui indique l'auteur. Anything after this will be changed to face.")

(defun +dark-get-quote (&optional nth)
  "Get a random quote from `+dark-quotes-file' Optionally get the NTH quote."
  (let* ((quotes (split-string
                  (with-temp-buffer
                    (insert-file-contents +dark-quotes-file)
                    (buffer-substring-no-properties
                     (point-min)
                     (point-max)))
                  +dark-quotes-file-seperator-regex t))
         (selected-quote (nth (or nth
                                  (random (length quotes)))
                              quotes)))
    (put-text-property (string-match +dark-quotes-author-regex selected-quote)
                       (length selected-quote) 'face 'font-lock-comment-face selected-quote)
    selected-quote))


(defun +dark-dashboard-widget-quotes ()
  "Insert my footer with my quote."
  (insert "\n\n")
  (dashboard-insert-center (+dark-get-quote)))

;; remplacement de la fonction insert-footer par la mienne dans DASHBOARD
;;(advice-add 'dashboard-insert-footer :override #'+dark-dashboard-widget-quotes)


(defvar +dark-enlight-footer
  (progn
    (+dark-get-quote))
  "Variable pour inserer la citation dans enlight.")


(package! grid
  :straight (:host github :repo "ichernyshovvv/grid.el"))

(require 'grid)

(defface enlight-green-bold
  '((t (:foreground "#9acd32" :bold t)))
  "Yellow bold face")

(defvar enlight-guix
  (propertize
   " ..                             `.
           `--..```..`           `..```..--`
             .-:///-:::.       `-:::///:-.
                ````.:::`     `:::.````
                     -//:`    -::-
                      ://:   -::-
                      `///- .:::`
                       -+++-:::.
                        :+/:::-
                        `-....`                "
   'face 'enlight-green-bold))


(package! enlight
  :demand t
  :init
  (setopt initial-buffer-choice #'enlight)
  ;;(evil-set-initial-state 'enlight-mode 'emacs)
  :custom
  (enlight-content
   (concat
    (grid-get-box `( :align center :content ,enlight-guix :width 60))
    (grid-get-row
     `(,(enlight-menu
         '(("Org Mode"
            ("Org-Agenda (current day)" (org-agenda nil "a") "a"))
           ("Downloads"
            ("Transmission" transmission "t")
            ("Downloads folder" (dired "~/Downloads") "d"))
           ("Other"
            ("Projects" project-switch-project "p"))))
       "   "
       ,(enlight-menu
         `(("Configuration"
            ("Jump to the config"
             ;; (progn
             ;;   (find-file ,+dark-conf-f)
             ;;   (goto-char (point-min)))
             "c")
            ("Bookmark" (list-bookmarks) "b"))
           ("Applications"
            ("Gnus" gnus "g")
            ("Elfeed" (elfeed) "e")
            ("Notdeft" (notdeft) "n")
            )))))

    (grid-get-column
     `((:content ,+dark-enlight-footer :width 60))))))
