(require 'core-straight)

  (package! corfu
    :custom
    ;; TAB cycle if there are only few candidates
    (completion-cycle-threshold nil)

    ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
    ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
    (read-extended-command-predicate
     #'command-completion-default-include-p)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (tab-always-indent 'complete)

    ;; Additional Customisations
    (corfu-cycle t)                     ;; Enable cycling for `corfu-next/previous'
    (corfu-auto t)                      ;; Enable auto completion
    (corfu-quit-no-match 'separator)    ;; Quit auto complete if there is no match
    (corfu-auto-prefix 2)               ;; Complete with less prefix keys
    (corfu-quit-at-boundary 'separator) ;; Never quit at completion boundary
    (corfu-preview-current nil)         ;; Disable current candidate preview
    (corfu-preselect 'directory)        ;; Preselect the fisrt canidate exept for directories select the prompt
    :preface
    ;; Completing in the minibuffer
    (defun dark-corfu-enable-always-in-minibuffer ()
      "Enable Corfu in the minibuffer if Vertico/Mct are not active."
      (unless (or (bound-and-true-p mct--active)
                  (bound-and-true-p vertico--input)
                  (eq (current-local-map) read-passwd-map))
        ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
        (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                    corfu-popupinfo-delay nil)
        (corfu-mode 1)))

    ;; https://github.com/minad/corfu/wiki#same-key-used-for-both-the-separator-and-the-insertion
    (defun dark-corfu-spc-handler ()
      (interactive)
      (if current-prefix-arg
          ;;we suppose that we want leave the word like that, so do a space
          (progn
            (corfu-quit)
            (insert " "))
        (if (and (= (char-before) corfu-separator)
                 (or
                  ;; check if space, return or nothing after
                  (not (char-after))
                  (= (char-after) ?\s)
                  (= (char-after) ?\n)))
            (progn
              (corfu-insert)
              (insert " "))
          (corfu-insert-separator))))
    :config
    ;; Free the RET key for less intrusive behavior.
    (keymap-unset corfu-map "RET")
    (when (fboundp 'straight-use-package)
      (add-to-list 'load-path
                   (expand-file-name "straight/build/corfu/extensions"
                                     straight-base-dir)))
    (require 'corfu-echo)
    (require 'corfu-history)
    (require 'corfu-popupinfo)
    (eldoc-add-command #'corfu-insert)
    :bind
    (("C-SPC"     . completion-at-point)
     :map corfu-map
     ("C-SPC"     . corfu-insert)
     ("<tab>"     . corfu-next)
     ("TAB"       . corfu-next)
     ("<backtab>" . corfu-previous)
     ("SPC"       . corfu-insert-separator)
     ("<escape>"  . corfu-quit))
    :hook
    ;; Recommended: Enable Corfu globally.
    ;; This is recommended since Dabbrev can be used globally (M-/).
    ;; See also `corfu-exclude-modes'.
    ((after-init . global-corfu-mode)
     (after-init . corfu-popupinfo-mode)
     (after-init . corfu-echo-mode)
     (after-init . corfu-history-mode)
     ;; disable auto completion for eshell, such that the completion behavior is similar to widely used shells like Bash, Zsh or Fish.
     (eshell-mode-hook . (lambda ()
                           (setq-local corfu-auto nil)
                           (corfu-mode)))
     ;; Enable minibuffer completion
     (minibuffer-setup . dark-corfu-enable-always-in-minibuffer)))

  (package! corfu-terminal
    :if (not (display-graphic-p))
    :after corfu
    :hook
    (global-corfu-mode . corfu-terminal-mode))
