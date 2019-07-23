(defvar bing-dict-cache-auto-save nil
  "Whether to save the bing dict cache when leaving emacs.")

(defvar bing-dict--cache nil
  "It will be used when offline, it's a association list.")

(defvar bing-dict-cache-file (expand-file-name "var/bing-dict/bing-dict-save.el" user-emacs-directory)
  "It is where cache will be stored")

(defvar bing-dict-cache-limit 100
  "The largest numbers of the items to be cached")

(defun bing-dict--cache-init ()
  "Initialize bing dict."
  (setq bing-dict--cache nil))

(defun bing-dict--cache-load ()
  (load bing-dict-cache-file 'noerror nil 'nosuffix))

(defun bing-dict--cache-overflow-p ()
  (> (length bing-dict--cache) bing-dict-cache-limit))

(defun bing-dict--update-cache ()
  "When `bing-dict--cache' overflow, update caches."
  (when (bing-dict--cache-overflow-p)
    ;; remove the least recently used one
    (let ((sorted-cache-list (sort bing-dict--cache (lambda (x1 x2)
                                                      (< (cddr x1) (cddr x2))))))
      (while (> (length sorted-cache-list) bing-dict-cache-limit)
        (setq sorted-cache-list (cdr sorted-cache-list)))
      (setq bing-dict--cache sorted-cache-list))))

(defun bing-dict--cache-save ()
  "Save bing dict cache."

  (let ((dir (file-name-directory bing-dict-cache-file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))

  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; save `bing-dict--cache' as S-expression
    (let (print-level print-length)
      (encode-coding-string
       (format "%S" `(setq bing-dict--cache ',bing-dict--cache))
       'utf-8
       nil
       (current-buffer))
      (let ((coding-system-for-write 'binary))
        (write-region nil nil  bing-dict-cache-file)))))

(defun bing-dict--maybe-save ()
  (when (and bing-dict--cache bing-dict-cache-auto-save)
    (bing-dict--cache-save)))

(provide 'bing-dict-cache)
