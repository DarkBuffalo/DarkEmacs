(require 'core-straight)

  (package! vertico
    :straight (:host github :repo "minad/vertico"
                     :files (:defaults "extensions/*")
                     :includes (vertico-buffer
                                vertico-directory
                                vertico-flat
                                vertico-indexed
                                vertico-mouse
                                vertico-quick
                                vertico-repeat
                                vertico-reverse))
    :custom
    ;; Show more candidates
    (vertico-count 20)

    ;; Grow and shrink the Vertico minibuffer
    (vertico-resize t)

    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    (vertico-cycle t)

    ;; Do not allow the cursor in the minibuffer prompt
    (minibuffer-prompt-properties
     '(read-only t cursor-intangible t face minibuffer-prompt))

    ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
    ;; Vertico commands are hidden in normal buffers.
    (read-extended-command-predicate
     #'command-completion-default-include-p)

    ;; Enable recursive minibuffers
    (enable-recursive-minibuffers t)

    ;; Enable Mouse support
    (vertico-mouse-mode t)
    :preface
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    :config
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; from: https://github.com/SystemCrafters/crafted-emacs/blob/c9ab29592b728954d3acc11d66e76cfbfbcb6189/modules/crafted-completion.el#L43
    ;; Straight and Package bundle the vertico package differently. When
    ;; using `package.el', the extensions are built into the package and
    ;; available on the load-path. When using `straight.el', the
    ;; extensions are not built into the package, so have to add that path
    ;; to the load-path manually to enable the following require.
    (when (fboundp 'straight-use-package)
      (add-to-list 'load-path
                   (expand-file-name "straight/build/vertico/extensions"
                                     straight-base-dir)))
    (require 'vertico-directory)

    :bind (:map vertico-map
                ("<return>"   . vertico-directory-enter)
                ("<S-return>" . vertico-exit-input))
    :hook
    ((minibuffer-setup . cursor-intangible-mode)
     (after-init . vertico-mode)))



(package! orderless
          :custom
          (completion-styles '(orderless basic))
          (completion-category-overrides '((file (styles basic partial-completion)))))



(package! consult
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap projectile-grep] . consult-grep)
  ("C-s" . consult-line)
  ("C-S-s" . consult-focus-lines)
  :config
  (setq completion-in-region-function #'consult-completion-in-region))

(package! marginalia
  :init
  (marginalia-mode))

(setq enable-recursive-minibuffers t)
