;;; tricks-n-gimmicks.el --- general nice configuration to have, it’ll help with later in the config -*- lexical-binding: t; -*-
;;; Commentary:
;; Loads all my tricks-n-gimmicks for a sweeter config.
;;; Code:
(require 'core-straight)
(require 'core-util)
(require 'core-paths)

;; ensure our config directories exist
;; since some programs will fail like `recentf' or `keyfreq'
(dolist (dir (list dark-local-dir dark-etc-dir dark-var-dir dark-cache-dir))
  (f-mkdir dir))


;; setup proper paths for *nix machines
;; https://emacs.stackexchange.com/questions/27918/why-is-exec-path-different-in-emacsclient-emacsserver-than-in-emacs
(with-os! (gnu/linux darwin)
  (package! exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))


;; ;; Check git access FIXME bug windows
;; (defconst dark--gh-access (string-prefix-p "Hi" (shell-command-to-string "ssh -T git@github.com"))
;;   "Checks if Emacs has ssh access for GitHub (inherited path).")
;; (defconst dark--gl-access (string-prefix-p "Welcome" (shell-command-to-string "ssh -T git@gitlab.com"))
;;   "Checks if Emacs has ssh access for GitLab (inherited path).")
;; (message "git acces")

;; ;; let's log only when it fails
;; (when (not dark--gh-access)
;;   (message "GH ACCESS: %s" dark--gh-access))
;; (when (not dark--gl-access)
;;   (message "GL ACCESS: %s" dark--gl-access))

;; (when (and shan--gh-access shan--gl-access) ; TODO: i wonder if it's possible to use the repo sources to determine to use ssh
;;   (setq straight-vc-git-default-protocol 'ssh))

;; custom settings
(defvar dark--preferred-logo 'logo
  "Preferred logo for dashboard startup.  If not found, use default.")



;; INFO Variable PERSO
(defconst dark--settings-path
  (expand-file-name "settings.el"
                    (expand-file-name "personal" user-emacs-directory))
  "Path to personal settings meant not be public (api keys and stuff).")

(defconst dark--settings-exist? (file-exists-p dark--settings-path)
  "Checks if dark--settings-path exists.")

(if dark--settings-exist?
    (load dark--settings-path nil 'nomessage)
  (message "Settings file not found!"))

;; From doom: these two functions don't exist in terminal Emacs, but some Emacs
;; packages (internal and external) use it anyway, leading to void-function
;; errors. I define a no-op substitute to suppress them.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))
(unless (fboundp 'set-fontset-font)
  (fset 'set-fontset-font #'ignore))

(after! whitespace
  (defun doom-is-childframes-p ()
    "`whitespace-mode' inundates child frames with whitespace markers, so
disable it to fix all that visual noise."
    (null (frame-parameter nil 'parent-frame)))
  (add-function :before-while whitespace-enable-predicate #'doom-is-childframes-p))

;;; Extra file extensions to support
(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)
   ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))

;;; Multiple languages can be useful when dealing with templates
(package! mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 0))

(provide 'tricks-n-gimmicks)
;;; tricks-n-gimmicks.el ends here
