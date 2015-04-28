;;; bing-dict.el --- Minimalists' English-Chinese Bing dictionary

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A minimalists' Emacs extension to search http://www.bing.com/dict. Support
;; English to Chinese and Chinese to English.

;; ## Setup

;;     (add-to-list 'load-path "/path/to/bing-dict.el")
;;     (require 'bing-dict)

;; ## Usage
;; You can call `bing-dict-brief` to get the explanations of you query. The results
;; will be shown in the minibuffer.

;; You should probably give this command a key binding:

;;     (global-set-key (kbd "C-c d") 'bing-dict-brief)

;; ## Using Firefox's Cookies Under Linux
;; Note this feature is only tested for Firefox 37 under Linux.

;; If you are using Firefox under Linux, you can search the word using your
;; Firefox's cookies. Why adding this feature? Because if you've logged into
;; `bing.com` in your Firefox and you've joined the
;; [Bing Rewards](https://www.bing.com/rewards/dashboard) program, then your search
;; within Emacs also counts towards the number of total searches you've performed,
;; just like you're using Firefox to perform the Bing search.

;; To enable this, use the following setting:

;;     (setq bing-dict-use-firefox-cookies t)

;; If your Firefox cookie file is not something like
;; `~/.mozilla/firefox/*.default/cookies.sqlite`, then you should set the value of
;; `bing-dict-firefox-cookies-file`.



;;; Code:

(require 'thingatpt)

(defvar bing-dict-sqlite-program "sqlite3")

(defvar bing-dict-use-firefox-cookies nil
  "Whether we should use firefox's cookies")

(defvar bing-dict-firefox-cookies-file
  "~/.mozilla/firefox/*.default/cookies.sqlite"
  "The path to the Firefox's cookie file")

(defun bing-dict--read-firefox-cookies ()
  (when (executable-find bing-dict-sqlite-program)
    (let* ((sqlite-file-cons (file-expand-wildcards
                              (expand-file-name
                               bing-dict-firefox-cookies-file)))
           (args
            (when sqlite-file-cons
              `("-batch"
                "-csv"
                ,(car sqlite-file-cons)
                "SELECT name, value FROM moz_cookies where host like \"%.bing.com%\""))))
      (with-temp-buffer
        (apply 'call-process bing-dict-sqlite-program nil t nil args)
        (replace-regexp-in-string
         ","
         "="
         (replace-regexp-in-string "\n" "; " (buffer-string)))))))

(defun bing-dict--replace-html-entities (str)
  (let ((retval str)
        (pair-list
         '(("&amp;" . "&")
           ("&hellip;" . "...")
           ("&quot;" . "\"")
           ("&#[0-9]*;" .
            (lambda (match)
              (format "%c" (string-to-number (substring match 2 -1))))))))
    (dolist (elt pair-list retval)
      (setq retval (replace-regexp-in-string (car elt) (cdr elt) retval)))))

(defun bing-dict--delete-response-header ()
  (save-excursion
    (ignore-errors
      (goto-char (point-min))
      (delete-region (point-min)
                     (1+ (re-search-forward "^$" nil t)))
      (goto-char (point-min)))))

(defun bing-dict--pronunciation ()
  (propertize
   (bing-dict--replace-html-entities
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward "prUS.*?\\(\\[.*?\\]\\)" nil t)
            (match-string-no-properties 1)
          (re-search-forward "hd_p1_1\" lang=\"en\">\\(.*?\\)</div" nil t)
          (match-string-no-properties 1)))))
   'face
   'font-lock-comment-face))

(defsubst bing-dict--clean-inner-html (html)
  (replace-regexp-in-string "<.*?>" "" html))

(defun bing-dict--definitions ()
  (let (defs)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward
                "span class=\"pos\">\\(.*?\\)</span>.*?<span class=\"def\">\\(.*?\\)</span></li>"
                nil
                t)
          (let ((pos (propertize (match-string-no-properties 1)
                                 'face
                                 'font-lock-doc-face))
                (def (match-string-no-properties 2)))
            (push (format "%s %s" pos def) defs)))
        (goto-char (point-min))
        (when (re-search-forward
               "span class=\"pos web\">\\(.*?\\)</span>.*?<span class=\"def\">\\(.*?\\)</span></li>"
               nil
               t)
          (let ((pos (propertize (match-string-no-properties 1)
                                 'face
                                 'font-lock-doc-face))
                (def (match-string-no-properties 2)))
            (push (format "%s %s" pos def) defs)))
        (mapcar 'bing-dict--clean-inner-html (nreverse defs))))))

(defun bing-dict--definitions-exist-p ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (not (re-search-forward "div class=\"smt_hw\"" nil t)))))

(defun bing-dict--has-result-p ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (not (re-search-forward "div class=\"no_results\"" nil t)))))

(defun bing-dict--machine-translation ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (re-search-forward "div class=\"p1-11\">\\(.*?\\)</div>" nil t)
        (bing-dict--clean-inner-html (match-string-no-properties 1))))))

(defun bing-dict-brief-cb (status keyword)
  (set-buffer-multibyte t)
  (bing-dict--delete-response-header)
  (if (bing-dict--has-result-p)
      (if (bing-dict--definitions-exist-p)
          (let ((query-word (propertize keyword 'face 'font-lock-keyword-face))
                (pronunciation (bing-dict--pronunciation))
                (short-exps (mapconcat 'identity (bing-dict--definitions)
                                       (propertize " | "
                                                   'face
                                                   'font-lock-builtin-face))))
            (message "%s %s: %s" query-word pronunciation short-exps))
        (message "Machine translation: %s --> %s" keyword
                 (propertize (bing-dict--machine-translation)
                             'face
                             'font-lock-doc-face)))
    (message "No results")))

;;;###autoload
(defun bing-dict-brief ()
  (interactive)
  (let* ((keyword (url-hexify-string
                   (read-string "Search Bing dict: "
                                (if mark-active
                                    (buffer-substring (region-beginning) (region-end))
                                  (word-at-point)))))
         (url-request-extra-headers
          (when bing-dict-use-firefox-cookies
            `(("Cookie" . ,(bing-dict--read-firefox-cookies))))))
    (url-retrieve (concat "http://www.bing.com/dict/search?q=" keyword)
                  'bing-dict-brief-cb
                  `(,(decode-coding-string (url-unhex-string keyword) 'utf-8))
                  t
                  t)))

(provide 'bing-dict)
;;; bing-dict.el ends here
