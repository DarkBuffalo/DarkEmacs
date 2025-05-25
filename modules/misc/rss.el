(require 'core-straight)


(defcustom +elfeed-videos-dir "~/Vidéos/elfeed/"
  "Directory of downloaded videos."
  :group 'dark-elfeed)
(defcustom +elfeed-images-dir "~/Images/elfeed/"
  "Directory of downloaded pictures."
  :group 'dark-elfeed)
(defcustom +yt-dlp-command "yt-dlp"
  "The \"yt-dlp\" command."
  :group 'dark-tools)
(defvar +elfeed-file "elfeed.org"
  "Name of org file.")


(package! emp
  :straight (:host github :repo "progfolio/emp"))

(package! elfeed
  :hook ((elfeed-search-mode . elfeed-update))
  :bind (:map dark-open-map
              ("f" . elfeed)
              :map elfeed-search-mode-map
              ("<tab>" . +elfeed-completing-filter)
              ("d" . +elfeed-youtube-dl)
              ("V" . +elfeed-play-w-mpv)
              ("w" . +elfeed-post-to-wallabag)
              :map elfeed-show-mode-map
              ("D" . +elfeed-download-image)
              ("w" . +elfeed-post-to-wallabag))

  :custom
  (elfeed-db-directory (concat dark-local-dir "elfeed/db/"))
  (elfeed-enclosure-default-dir (concat dark-local-dir "elfeed/enclosure/"))
  :config
  ;; Hide the annoying index file form recent files
  ;;(+ignore-root elfeed-db-directory elfeed-enclosure-default-dir)

  (defun +elfeed-post-to-wallabag (entries)
    (interactive (list (pcase major-mode
                         ('elfeed-search-mode
                          (elfeed-search-selected))
                         ('elfeed-show-mode
                          (list elfeed-show-entry)))))
    (dolist (entry (ensure-list entries))
      (wallabag-url (elfeed-entry-link entry))))

  (defun +elfeed-download-image ()
    "Download the image at point."
    (interactive)
    (let ((url (get-text-property (point) 'image-url)))
      (if (not url)
          (message "No image at point!")
        (url-copy-file
         url (expand-file-name (url-file-nondirectory url)
                               (+directory-ensure +elfeed-images-dir))))))

  (defun +elfeed-play-w-mpv ()
    "Play selected videos in a shared mpv instance in chronological order."
    (interactive)
    (mapc (lambda (entry)
            (emp-open-url (elfeed-entry-link entry))
            (message "Playing %S in MPV" (elfeed-entry-title entry)))
          (nreverse (elfeed-search-selected)))
    (elfeed-search-untag-all-unread))

  (defun +yt-dl-it (url)
    "Downloads the URL with \"yt-dlp\" in an async shell."
    (let ((default-directory (+directory-ensure +elfeed-videos-dir)))
      (async-shell-command (format "%s '%s'" +yt-dlp-command url))))

  (defun +elfeed-youtube-dl (&optional use-generic-p)
    "Download Youtube videos."
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (+yt-dl-it it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun +elfeed-filter-results-count (search-filter)
    "Count results for SEARCH-FILTER."
    (let* ((filter (elfeed-search-parse-filter search-filter))
           (head (list nil))
           (tail head)
           (count 0))
      (let ((lexical-binding t)
       (func (byte-compile (elfeed-search-compile-filter filter))))
   (with-elfeed-db-visit (entry feed)
                         (when (funcall func entry feed count)
                           (setf (cdr tail) (list entry)
                                 tail (cdr tail)
                                 count (1+ count)))))
      count))

  (defun +elfeed-view-filtered (filter)
    "Filter the elfeed-search buffer to show feeds tagged with FILTER."
    (interactive)
    (elfeed)
    (unwind-protect
        (let ((elfeed-search-filter-active :live))
          (setq elfeed-search-filter filter))
      (elfeed-search-update :force)))

  (defun +elfeed-get-tags ()
    (let ((all-tags '()))
      (with-temp-buffer
        (insert-file-contents
         (concat darkemacs-assets-dir +elfeed-file)
         (delay-mode-hooks (org-mode)))
        (org-map-entries (lambda ()
                           (let ((tag-string (car (last (org-heading-components)))))
                             (when tag-string
                               (setq all-tags
                                     (append all-tags (split-string tag-string ":" t)))))))

        ;; now get counts
        (cl-loop for tag in (seq-uniq all-tags)
                 collect  (cons tag (format "@1months-ago +unread +%s" tag))))))

  (defun +elfeed-completing-filter ()
    "Completing filter."
    (interactive)
    (let* ((tags (append '(("All" . "@1-months-ago +unread")) (+elfeed-get-tags) nil))
           (categories (-filter
                        (lambda (item)
                          (> (+elfeed-filter-results-count (cdr item))
                             0))
                        tags)))

      (if ( > (length categories) 0)
          (progn
            (+elfeed-view-filtered (cdr (assoc (completing-read "Categories: " categories)
                                               categories)))
            (goto-char (window-start)))
        (message "All caught up \\o/")))))

(package! elfeed-org
  :after (elfeed)
  :init
  (setq rmh-elfeed-org-files (list (concat dark-assets-dir +elfeed-file)))
  (elfeed-org))

(defun darkemacs-elfeed-capture-entry ()
  "Capture selected entries into inbox."
  (interactive)
  (elfeed-search-tag-all 'opened)
  (previous-logical-line)
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
        do (elfeed-untag entry 'unread)
        when (elfeed-entry-link entry)
        do (cl-flet ((raise-frame nil nil))
    (org-protocol-capture (list :template "B"
                                :url it
                                :title (format "%s: %s"
                                               (elfeed-feed-title (elfeed-entry-feed entry))
                                               (elfeed-entry-title entry))
                                              :elfeed-data entry))))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))
