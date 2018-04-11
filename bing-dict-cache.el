(defvar bing-dict-cache-auto-save t
  "Whether to save the bing dict cache when leaving emacs.")

(defvar bing-dict--cache nil
  "It will be used when offline, its type is a hash table")

(defvar bing-dict-cache-file (expand-file-name "var/bing-dict/bing-dict-save.el" user-emacs-directory)
  "It is where cache will be stored")

(defvar bing-dict-cache-limit 100
  "The largest numbers of the items to be cached")

(defun bing-dict--cache-init ()
  "Initialize bing dict."
  (setq bing-dict--cache (make-hash-table :test #'equal)))

(defun bing-dict--cache-initialized-p ()
  (hash-table-p bing-dict--cache))

(defun bing-dict--cache-load ()
  (load bing-dict-cache-file 'noerror nil 'nosuffix))

(defun bing-dict--cache-overflow-p ()
  (> (hash-table-count bing-dict--cache) bing-dict-cache-limit))

(defun bing-dict--update-cache ()
  "When `bing-dict--cache' overflow, update caches."
  (when (bing-dict--cache-overflow-p)
    (let (cache-list)
      ;; hash-table to list, so that we can sort it depend on time of updating
      (maphash
       (lambda (key value)
         (push (cons key value) cache-list))
       bing-dict--cache)
      ;; remove the least recently used one
      (let ((sorted-cache-list (sort cache-list (lambda (x1 x2)
                                                  (< (cddr x1) (cddr x2))))))
        (while (> (length sorted-cache-list) bing-dict-cache-limit)
          (remhash (caar sorted-cache-list)
                   bing-dict--cache)
          (setq sorted-cache-list (cdr sorted-cache-list)))))))

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
       (format "%S" `(setq bing-dict--cache ,bing-dict--cache))
       'utf-8
       nil
       (current-buffer))
      (let ((coding-system-for-write 'binary))
        (write-region nil nil  bing-dict-cache-file)))))

(defun bing-dict--maybe-save ()
  (when (and (bing-dict--cache-initialized-p) bing-dict-cache-auto-save)
    (bing-dict--cache-save)))

(provide 'bing-dict-cache)
