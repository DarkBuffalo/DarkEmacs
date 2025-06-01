;;; core-straight.el --- package manager -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'core-paths)

;; ;; HACK: I have no idea what this is, but straight needs it 'defined'
;; ;; I think it was renamed but there's a discrepancy between straight and emacs
;; (defvar native-comp-deferred-compilation-deny-list
;;   (bound-and-true-p native-comp-async-env-modifier-form))

;; (defconst straight-repository-branch "master") ; more stable branch
;; (defconst straight-use-package-by-default t)
;; (defconst straight-recipe-repositories nil)

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; ;; TODO: I still have some stray demands in my config, should figure that out sometime
;; ;; but really the config should be redone to utilize deferring logic
;; (setq-default use-package-always-defer nil
;;               use-package-always-demand t
;;               byte-compile-warnings nil)
;; ;; (setq use-package-verbose t)

;; (require 'straight)
;; (straight-use-package 'use-package)
;; (require 'use-package)


;; ;; abstract straight so it can be observed and potentially hot swappable later
 (defvar core-straight--loaded-packages '()
   "List containing loaded packages.")



;; Optimize Startup Time

;; Inspiré par :

;; - https://gist.github.com/axyz/76871b404df376271b521212fba8a621
;; - https://github.com/alexluigit/dirvish/blob/main/docs/.emacs.d.example/early-init.el
;; - https://github.com/jamescherti/minimal-emacs.d/blob/main/early-init.el
;; - https://github.com/mnewt/dotemacs/blob/master/early-init.el
;; - https://github.com/nilcons/emacs-use-package-fast#a-trick-less-gc-during-startup


;; We're going to increase the gc-cons-threshold to a very high number to decrease the load time and add a hook to measure Emacs startup time.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; Let's lower our GC thresholds back down to a sane level.
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold (* 16 1024 1024))))

;; Profile emacs startup
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "*** Emacs loaded in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; remove "for information about gnu emacs..." message at startup
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; suppress the vanilla startup screen completely. we've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)

;; never show the hello file
(defalias #'view-hello-file #'ignore)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased".
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

;; Unset `file-name-handler-alist' too (temporarily). Every file opened and
;; loaded by Emacs will run through this list to check for a proper handler for
;; the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)))

;; Remove irreleant command line options for faster startup
(setq command-line-x-option-alist nil)

;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Set eln-cache dir [[https://emacs.stackexchange.com/questions/70449/set-custom-location-for-eln-cache][emacs stack solution]]
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" dark-var-dir)))

;; Configure Byte Compile

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Disable certain byte compiler warnings to cut down on the noise.
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; DOOM runtime optimizations
;; The following optimizations have been taken from [[https://github.com/doomemacs/doomemacs/blob/da3d0687c5008edbbe5575ac1077798553549a6a/lisp/doom-start.el#L30][here]].

;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other

(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
;;(setq idle-update-delay 1.0)  ; default is 0.5
(setq wich-func-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Configure Straight
;; This section provides the bootstrap code for =straight.el=, a package manager for Emacs.
;; The code includes optimization for startup time, disables file modification checking for performance, and loads the =straight.el= bootstrap file, which contains essential functionality.


(defvar straight-base-dir)

;; prevent package.el loading packages prior to their init-file loading.
(setq straight-base-dir dark-local-dir
      package-quickstart nil
      package-enable-at-startup nil)

;; straight.el bootstrap code
;;disable checking (for speedup).
(setq straight-check-for-modifications nil)

;; cache the autoloads of all used packages in a single file
(setq straight-cache-autoloads t)

;; Enable straight use-package integration
(setq straight-use-package-by-default t
      use-package-always-defer t)

;;; Use-package modifications
;; from doom
(setq use-package-compute-statistics init-file-debug
      use-package-verbose init-file-debug
      use-package-minimum-reported-time (if init-file-debug 0 0.1)
      use-package-expand-minimally (not noninteractive))

(defvar bootstrap-version)
(let ((bootstrap-file (concat straight-base-dir "straight/repos/straight.el/bootstrap.el"))
      (install-url "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el")
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously install-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))




(defmacro package! (name &rest args)
  "Like `use-package', but cooler since it also tracks which packages loaded.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  (add-to-list 'core-straight--loaded-packages name)
  `(use-package ,name ,@args))

(provide 'core-straight)
;;; core-straight.el ends here
