

(defcustom dark-lisp-project-licence "BSD-3"
  "The default licence used for new projects created with `create-project'."
  :type 'string
  :group 'dark-lisp)

;;; Toolkit functions
(defun dark-fwrite (contents file &optional append)
  (write-region contents nil file append))

(defun dark-fread (file)
  (with-temp-buffer
      (insert-file-contents file)
    (buffer-string)))

(defun replace-project-variables (string &optional vars)
  (let ((vars (or vars
                  '(user-full-name user-mail-address
                                   project-name project-description
                                   project-licence year month day)))
        (year (format-time-string "%Y"))
        (month (format-time-string "%m"))
        (day (format-time-string "%d"))
        ;; Case-sensitive regexp matching
        (case-fold-search nil))
    (dolist (var vars string)
      (setq string (replace-regexp-in-string (upcase (symbol-name var)) (symbol-value var) string t t)))))

(defun --copy-project-internal (from to)
  (make-directory to t)
  (let ((project-licence dark-lisp-project-licence))
    (dolist (template (directory-files from))
      (unless (or (string= template ".")
                  (string= template ".."))
        (let ((srcfile (concat from "/" template))
              (destfile (concat to "/" (replace-project-variables template))))
          (cond ((file-directory-p srcfile)
                 (--copy-project-internal srcfile destfile))
                (t
                 (dark-fwrite (replace-project-variables (dark-fread srcfile))
                                  destfile))))))))


(defun maybe-update-quicklisp-db ()
  (interactive)
  (cond ((slime-connected-p)
         (message "Updating Quicklisp DB...")
         (slime-eval '(ql:register-local-projects))
         (message "Quicklisp DB updated."))
        (t
         (message "Slime not connected, cannot update."))))

(cl-defun lisp-create-project (&key name description licence)
  "Create a lisp project from skeleton."
  (interactive)
  (let* ((project-name (or name (read-string "Project name: ")))
         (project-description (or description (read-string "Project description: ")))
         (project-licence (or licence (read-string "Project licence: " dark-lisp-project-licence)))
         (dir  (concat (expand-file-name "projects/" dark-lisp-dir) project-name))
         (skeleton (expand-file-name "config/skeleton/" dark-lisp-dir)))
    (cond ((file-exists-p dir)
           (message "A project with that name already exists."))
          (t
           (message "Creating project skeleton...")
           (--copy-project-internal skeleton dir)
           (magit-init dir)
           (maybe-update-quicklisp-db)
           (message "Project created.")))))



(defmacro define-lisp-implementations (&rest decl)
  `(progn
     ,@(cl-loop for (symbol . args) in decl
                collect `(progn
                           (defun ,symbol ()
                             (interactive)
                             (slime ',symbol))
                           (cl-pushnew '(,symbol ,@args) slime-lisp-implementations
                                       :key 'car)))))

(after! org
  (add-to-list 'org-babel-load-languages '(lisp . t)))

(package! suggest
  :defer t)


;; (package! lispy
;;   :hook ((emacs-lisp-mode . lispy-mode)
;;          (lisp-mode . lispy-mode)
;;          (clojure-mode . lispy-mode)
;;          (scheme-mode . lispy-mode)
;;          (sly-mrepl-mode . lispy-mode)))

(with-feature! +slime
  (package! slime
    :mode
    ("\\.sexp\\'" . common-lisp-mode)
    ("\\.lisp\\'" . common-lisp-mode)
    ("\\.asd\\'" . common-lisp-mode)
    :config
    (setq tab-width 4)
    (setq inferior-lisp-program "sbcl")
    (slime-setup '(slime-repl
                   slime-indentation
                   slime-quicklisp
                   slime-asdf
                   slime-fancy
                   slime-sbcl-exts
                   slime-banner))
    ;; :hook ((lisp-mode . slime))
    :custom
    (slime-contribs '(slime-asdf slime-autodoc
                                 slime-cl-indent slime-compiler-notes-tree
                                 slime-fontifying-fu slime-fuzzy
                                 slime-hyperdoc slime-indentation
                                 slime-macrostep slime-mdot-fu
                                 slime-quicklisp slime-references
                                 slime-repl slime-sprof slime-trace-dialog
                                 slime-tramp slime-xref-browser))
    (slime-completion-at-point-functions '(slime-filename-completion slime-fuzzy-complete-symbol))
    (slime-net-coding-system 'utf-8-unix)
    (slime-startup-animation nil)
    (slime-auto-select-connection 'always)
    (slime-kill-without-query-p t)
    (slime-description-autofocus t )
    (slime-fuzzy-explanation "")
    (slime-asdf-collect-notes t)
    (slime-inhibit-pipelining nil)
    (slime-load-failed-fasl 'always)
    (slime-when-complete-filename-expand t)
    (slime-export-symbol-representation-auto t)
    (initial-major-mode 'common-lisp-mode)
    (initial-scratch-message ";; Scratch Common Lisp"))


  (after! slime
    (when (or (eq system-type 'gnu/linux)
              (eq system-type 'darwin))
      (define-lisp-implementations
       (sbcl  ("sbcl" "--dynamic-space-size" "8192"))
       ;; (abcl  ("abcl"))
       ;; (acl   ("alisp"))
       (ccl   ("ccl"))
       ;; (clasp ("clasp"))
       (clisp ("clisp"))
       ;; (cmucl ("cmucl" "-quiet"))
       (ecl   ("ecl"))
       ;; (mkcl  ("mkcl"))
       ;; (xcl   ("xcl"))
       (sbcl-win  ("sbcl-win" "--dynamic-space-size" "8192"))))

    (when (eq system-type 'windows-nt)
      (define-lisp-implementations
       ;; (ccl   ("wx86cl64.exe"))
       ;; (ccl32 ("wx86cl.exe"))
       (clisp ("clisp.exe"))
       (sbcl  ("sbcl.exe" "--dynamic-space-size" "8192"))
       )))

  ) ;; end feature slime



(with-feature! +sly
  (package! sly
    :mode (("\\.lisp$" . sly-mode))
    :hook ((sly-mode . rainbow-delimiters-mode)
           (sly-mode . smartparens-strict-mode))
    :defer t
    :config
    (setq inferior-lisp-program "sbcl"))


  (package! sideline-sly
    :straight (:host github :repo "emacs-sideline/sideline-sly")
    :hook (sideline-mode . sideline-sly-setup)
    :init
    (setq sideline-backends-right '(sideline-sly)))


  (package! sly-asdf
    :after sly
    :init
    (add-to-list 'sly-contribs 'sly-asdf))

  (package! sly-quicklisp
    :after sly)

  (package! sly-macrostep
    :after sly)

  (package! sly-repl-ansi-color
    :after sly
    :init
    (add-to-list 'sly-contribs 'sly-repl-ansi-color))

  (package! sly-overlay
    :after sly))
;; end feature sly
