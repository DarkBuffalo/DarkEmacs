(require 'core-straight)

(package! kimai
  :straight (:local-repo "~/Dropbox/Git/kimai"))


(defun +chronos-notify (c)
  "Notify expiration of timer C using custom script."
  (chronos--shell-command "Chronos notification"
                          "chronos-notify"
                          (list (chronos--time-string c)
                                (chronos--message c))))

;; (package! chronos
;;   :straight (:local-repo "~/Dropbox/Git/chronos/")
;;   :demand
;;   :bind
;;   (:map my/open-map ;; TODO: modifier par la bonne variable de keybinds
;;         ("c". chronos-select-timer))
;;   :config
;;   (setq chronos-standard-timers
;;         '("Pomodoro                    25/Finish and Reflect + 5/Au boulot !"
;;           "Break                       30/Au boulot !"
;;           "Class: Very Short            1/End"
;;           "Class: Short                 5/End"
;;           "Class: Medium               10/End"
;;           "Class: Long                 15/End"))

;;   (setq chronos-expiry-functions '(+chronos-notify))

;;   (if os/win ;; TODO: modifier par ce qui fonctionne
;;       (setq chronos-text-to-speech-program "espeak-ng") ;; ne pas oublier d'installer espeak-ng - https://github.com/espeak-ng/espeak-ng
;;     (setq chronos-text-to-speech-program "espeak"))

;;   (setq chronos-shell-notify-program "mpv"
;;         chronos-shell-notify-parameters '("--really-quiet"
;;                                           "--af=scaletempo=speed=pitch"
;;                                           "--speed=0.65"
;;                                           "~/Dropbox/Git/emacs.dark/assets/songs/lofibell.wav")

;;         chronos-text-to-speech-program-parameters "-s 100"
;;         chronos-expiry-functions '(chronos-dunstify
;;                                    chronos-buffer-notify
;;                                    chronos-shell-notify
;;                                    chronos-text-to-speech-notify)))


(with-feature! +vundo
  (package! vundo))
