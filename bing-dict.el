;;; bing-dict.el --- Minimalists' English-Chinese Bing dictionary

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; URL: https://github.com/cute-jumper/bing-dict.el
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

;;; Code:

(require 'thingatpt)

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
  (let (has-result)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (setq has-result (not (re-search-forward "div class=\"no_results\"" nil t)))
        (when has-result
          (goto-char (point-min))
          (setq has-result
                (not (re-search-forward "div class=\"df_wb_a\">Sounds like</div>" nil t))))
        has-result))))

(defun bing-dict--machine-translation ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (re-search-forward "div class=\"p1-11\">\\(.*?\\)</div>" nil t)
        (bing-dict--clean-inner-html (match-string-no-properties 1))))))

(defun bing-dict-brief-cb (status keyword)
  (set-buffer-multibyte t)
  (bing-dict--delete-response-header)
  (condition-case nil
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
        (message "No results"))
    (error (message "No results"))))

;;;###autoload
(defun bing-dict-brief (&optional word)
  (interactive)
  (let ((keyword (or word (read-string
                           "Search Bing dict: "
                           (if (use-region-p)
                               (buffer-substring (region-beginning) (region-end))
                             (thing-at-point 'word t))))))
    (url-retrieve (concat "http://www.bing.com/dict/search?q=" (url-hexify-string keyword))
                  'bing-dict-brief-cb
                  `(,(decode-coding-string keyword 'utf-8))
                  t
                  t)))

(provide 'bing-dict)
;;; bing-dict.el ends here
