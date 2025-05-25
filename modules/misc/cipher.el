;;;###autoload
;; (defun rot47 (text)
;;   "Crypte et decrypte TEXT en utilisant l'algorithme ROT47."
;;   (interactive)
;;   (apply 'string
;;          (mapcar (lambda (char)
;;                    (if (and (>= char 33) (<= char 126))
;;                        (+ 33 (mod (+ (- char 33) 47) 94))
;;                      char))
;;                  text)))


(defconst rot47-translate-table
  (let ((table (make-char-table 'translation-table)))
    (dotimes (i 94)
      (aset table (+ i 33) (+ 33 (mod (+ i 47) 94))))
    table)
  "Char table for ROT47 translation.")

;;;###autoload
(defun rot47 (object &optional start end)
  "ROT47 encrypt OBJECT, a buffer or string.
If OBJECT is a buffer, encrypt the region between START and END.
If OBJECT is a string, encrypt it in its entirety, ignoring START
and END, and return the encrypted string."
  (if (bufferp object)
      (with-current-buffer object
        (rot47-region start end))
    (rot47-string object)))

;;;###autoload
(defun rot47-string (string)
  "Return ROT47 encryption of STRING."
  (with-temp-buffer
    (insert string)
    (rot47-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun rot47-region (start end)
  "ROT47 encrypt the region between START and END in current buffer."
  (interactive "r")
  (condition-case nil
      (translate-region start end rot47-translate-table)
    (buffer-read-only
     (when (called-interactively-p 'interactive)
       (let ((dec (rot47-string (buffer-substring start end))))
         (message "Buffer is read-only:\n%s" (string-trim dec)))))))
