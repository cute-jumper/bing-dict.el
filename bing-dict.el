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

;; A **minimalists'** Emacs extension to search http://www.bing.com/dict.
;; Support English to Chinese and Chinese to English.

;; 极简主义者的 Emacs 必应词典。 支持中英互译。

;; ## Setup

;; You can install via [melpa](http://melpa.org).

;; If installing this package manually:

;;     (add-to-list 'load-path "/path/to/bing-dict.el")
;;     (require 'bing-dict)

;; ## Usage
;; You can call `bing-dict-brief` to get the explanations of you query. The results
;; will be shown in the echo area.

;; Here is the screenshot:

;; ![bing-dict-screenshot1](./screenshot1.png)

;; You should probably give this command a key binding:

;;     (global-set-key (kbd "C-c d") 'bing-dict-brief)

;; ## Customization
;; You can set the value of `bing-dict-add-to-kill-ring` to control whether the
;; result should be added to the `kill-ring` or not. By default, the value is
;; `nil`. If set to `t`, the result will be added to the `kill-ring` and you are
;; able to use `C-y` to paste the result.

;; Also, sometimes synonyms and antonyms could be useful, set
;; `bing-dict-show-thesaurus` to control whether you need them or not. The value of
;; `bing-dict-show-thesaurus` could be either `nil`, `'synonym`, `'antonym` or
;; `'both`. The default value is `nil`. Setting the vaule to `'synonym` or
;; `'antonym` only shows the corresponding part, and setting it to `'both` will
;; show both synonyms and antonyms at the same time:

;;     (setq bing-dict-show-thesaurus 'both)

;; The variable `bing-dict-pronunciation-style` controls how the pronunciation is
;; shown. By default, its value is `'us` and the pronunciation is shown using
;; "American Phonetic Alphabet" (APA). You can choose the "International Phonetic
;; Alphabet" (IPA) by setting its value to `'uk` (In fact, any value other than
;; `'us` will work):

;;     (setq bing-dict-pronunciation-style 'uk)

;; You can also build your own vocabulary by saving all your queries and their
;; results into `bing-dict-org-file` (which points to
;; `~/.emacs.d/bing-dict/vocabulary.org` by default):

;;     (setq bing-dict-save-search-result t)

;; By setting `bing-dict-org-file`, you can change where all the queries and
;; results are saved:

;;     (setq bing-dict-org-file "/path/to/your_vocabulary.org")

;; screenshot:

;; ![bing-dict-screenshot2](./screenshot2.png)

;; ## For Features Requests
;; This extension aims for a quick search for a word. Currently this extension only
;; parses several sections in the search results and show a brief message in the
;; echo area using `bing-dict-brief`. I don't plan to write parsers for all the
;; sections of the search results. At least, for `bing-dict-brief`, it should not
;; present too much information which may not fit into the echo area.

;; If you want to view the complete results of your query word, there are two
;; options: using the external browser to do this or contributing to the repo by
;; adding more parsers. For the first option, the following code could partly
;; achieve the goal:

;;     (browse-url
;;      (concat "http://www.bing.com/dict/search?mkt=zh-cn&q="
;;            (url-hexify-string
;;             (read-string "Query: "))))

;; If you prefer to browse inside Emacs, use `eww` instead:

;; ```
;; (eww-browse-url
;;   (concat "http://www.bing.com/dict/search?mkt=zh-cn&q="
;;           (url-hexify-string
;;            (read-string "Query: "))))
;; ```

;; Or open the web page in other window:

;; ```
;; (switch-to-buffer-other-window
;;  (eww-browse-url
;;   (concat "http://www.bing.com/dict/search?mkt=zh-cn&q="
;;           (url-hexify-string
;;            (read-string "Query: ")))))
;; ```

;; For the second option, you're welcome to contribute to this extension by adding
;; more parsers. For example, you could try to add a parser for the "Sample
;; Sentences" section. If you're able to write parsers to parse many sections,
;; which turns out to be too large to be shown in the echo area, you should
;; probably define a new command, maybe called `bing-dict-complete`. I'm totally OK
;; with new commands that could present more results, as long as `bing-dict-brief`
;; remains its original simplicity.


;;; Code:

(require 'thingatpt)

(defvar bing-dict-pronunciation-style 'us
  "Pronuciation style.
If the value is set to be `us', use the US-style pronuciation.
Otherwise, use the UK-style.")

(defvar bing-dict-show-thesaurus nil
  "Whether to show synonyms, antonyms or not.
The value could be `synonym', `antonym', `both', or nil.")

(defvar bing-dict-add-to-kill-ring nil
  "Whether the result should be added to `kill-ring'.")

(defvar bing-dict-org-file (expand-file-name "bing-dict/vocabulary.org" user-emacs-directory)
  "The file where store the vocabulary.")

(defvar bing-dict-org-file-title "Vocabulary"
  "The title of the vocabulary org file.")

(defvar bing-dict-save-search-result nil
  "Save bing dict search result or not.")

(defvar bing-dict-word-def-separator ": "
  "Seperator used between the word and the definition.")

(eval-when-compile
  (declare-function org-insert-heading "org")
  (declare-function org-insert-subheading "org"))

(defvar bing-dict-history nil)

(defvar bing-dict--base-url "http://www.bing.com/dict/search?mkt=zh-cn&q=")
(defvar bing-dict--no-resul-text (propertize "No results"
                                             'face
                                             'font-lock-warning-face))
(defvar bing-dict--machine-translation-text (propertize "Machine translation"
                                                        'face
                                                        'font-lock-builtin-face))
(defvar bing-dict--sounds-like-text (propertize "Sounds like"
                                                'face
                                                'font-lock-builtin-face))
(defvar bing-dict--separator (propertize " | "
                                         'face
                                         'font-lock-builtin-face))

(defun bing-dict--tidy-headlines ()
  "Remove extra spaces between stars and the headline text."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+\\([[:space:]][[:space:]]+\\)" nil t)
      (replace-match " " nil nil nil 1))))

(defun bing-dict--save-word (word definition)
  "Save WORD and DEFINITION in org file.  If there is already the same WORD, ignore it."
  (let ((dir (file-name-directory bing-dict-org-file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-buffer
    (when (file-exists-p bing-dict-org-file)
      (insert-file-contents bing-dict-org-file))
    (org-mode)
    (goto-char (point-min))
    (unless (re-search-forward (concat "^\\* " bing-dict-org-file-title) nil t)
      (beginning-of-line)
      (org-insert-heading)
      (insert bing-dict-org-file-title)
      (goto-char (point-min)))
    (unless (re-search-forward (concat "^\\*+ \\b" (car (split-string word)) "\\b") nil t)
      (end-of-line)
      (org-insert-subheading t)
      (insert word)
      (newline)
      (insert definition))
    (write-region nil nil bing-dict-org-file)))

(defun bing-dict--message (format-string &rest args)
  (let ((result (apply #'format format-string args)))
    (when bing-dict-save-search-result
      (let ((plain-result (substring-no-properties result)))
        (unless (or (string-match bing-dict--sounds-like-text plain-result)
                    (string-match bing-dict--no-resul-text plain-result))
          (let ((word (car (split-string plain-result
                                         bing-dict-word-def-separator)))
                (definition (nth 1 (split-string plain-result
                                                 bing-dict-word-def-separator))))
            (and word
                 definition
                 (bing-dict--save-word word definition))))))
    (when bing-dict-add-to-kill-ring
      (kill-new result))
    (message result)))

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
  (ignore-errors
    (goto-char (point-min))
    (delete-region (point-min)
                   (1+ (re-search-forward "^$" nil t)))
    (goto-char (point-min))))

(defun bing-dict--pronunciation ()
  (let ((pron-regexp (concat "<div class=\"hd_pr"
                             (and (eq bing-dict-pronunciation-style 'us)
                                  "US")
                             "\"")))
    (propertize
     (bing-dict--replace-html-entities
      (or
       (progn
         (goto-char (point-min))
         (if (re-search-forward pron-regexp nil t)
             (progn
               (goto-char (point-min))
               (when (re-search-forward (concat pron-regexp "[^[]*\\(\\[.*?\\]\\)") nil t)
                 (match-string-no-properties 1)))
           (when (re-search-forward "hd_p1_1\" lang=\"en\">\\(.*?\\)</div" nil t)
             (match-string-no-properties 1))))
       ""))
     'face
     'font-lock-comment-face)))

(defsubst bing-dict--clean-inner-html (html)
  (replace-regexp-in-string "<.*?>" "" html))

(defun bing-dict--definitions ()
  (let (defs)
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
    (mapcar 'bing-dict--clean-inner-html defs)))

(defun bing-dict--thesaurus (header starting-regexp)
  (let (thesaurus)
    (goto-char (point-min))
    (when (re-search-forward starting-regexp nil t)
      (catch 'break
        (while t
          (re-search-forward
           "div class=\"de_title1\">\\(.*?\\)</div><div class=\"col_fl\">\\(.*?\\)</div>"
           nil t)
          (push (format "%s %s"
                        (propertize (match-string-no-properties 1) 'face 'font-lock-string-face)
                        (bing-dict--clean-inner-html
                         (match-string-no-properties 2)))
                thesaurus)
          (goto-char (match-end 0))
          (unless (looking-at "</div><div class=\"df_div2\">")
            (throw 'break t))))
      (format "%s %s"
              (propertize header 'face 'font-lock-doc-face)
              (mapconcat #'identity thesaurus " ")))))

(defun bing-dict--synonyms ()
  (bing-dict--thesaurus "Synonym" "div id=\"synoid\""))

(defun bing-dict--antonyms ()
  (bing-dict--thesaurus "Antonym" "div id=\"antoid\""))

(defun bing-dict--has-machine-translation-p ()
  (goto-char (point-min))
  (re-search-forward "div class=\"smt_hw\"" nil t))

(defun bing-dict--machine-translation ()
  (goto-char (point-min))
  (when (re-search-forward "div class=\"p1-11\">\\(.*?\\)</div>" nil t)
    (propertize
     (bing-dict--clean-inner-html (match-string-no-properties 1))
     'face
     'font-lock-doc-face)))

(defun bing-dict--get-sounds-like-words ()
  (goto-char (point-min))
  (when (re-search-forward "div class=\"web_div\">\\(.*?\\)<div class=\"\\(dym_area\\|dymp_sm_top\\)\"" nil t)
    (let ((similar-words "")
          (content (match-string-no-properties 1)))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "<a.*?>\\(.*?\\)</a><div.*?>\\(.*?\\)</div>" nil t)
          (setq similar-words (concat similar-words
                                      (propertize (match-string-no-properties 1)
                                                  'face
                                                  'font-lock-keyword-face)
                                      " "
                                      (match-string-no-properties 2)
                                      "; ")))
        similar-words))))

(defun bing-dict-brief-cb (status keyword)
  (set-buffer-multibyte t)
  (bing-dict--delete-response-header)
  (setq keyword (propertize keyword
                            'face
                            'font-lock-keyword-face))
  (condition-case nil
      (if (bing-dict--has-machine-translation-p)
          (bing-dict--message "%s%s%s -> %s"
                              bing-dict--machine-translation-text
                              bing-dict-word-def-separator
                              keyword
                              (bing-dict--machine-translation))
        (let ((defs (bing-dict--definitions))
              extra-defs
              pronunciation
              short-defstr)
          (if defs
              (progn
                (cond
                 ((eq bing-dict-show-thesaurus 'synonym)
                  (when (setq extra-defs (bing-dict--synonyms))
                    (push extra-defs defs)))
                 ((eq bing-dict-show-thesaurus 'antonym)
                  (when (setq extra-defs (bing-dict--antonyms))
                    (push extra-defs defs)))
                 ((eq bing-dict-show-thesaurus 'both)
                  (dolist (func '(bing-dict--synonyms bing-dict--antonyms))
                    (when (setq extra-defs (funcall func))
                      (push extra-defs defs)))))
                (setq
                 pronunciation (bing-dict--pronunciation)
                 short-defstr (mapconcat 'identity (nreverse defs)
                                         bing-dict--separator))
                (bing-dict--message "%s %s%s%s"
                                    keyword
                                    pronunciation
                                    bing-dict-word-def-separator
                                    short-defstr))
            (let ((sounds-like-words (bing-dict--get-sounds-like-words)))
              (if sounds-like-words
                  (bing-dict--message "%s%s%s"
                                      bing-dict--sounds-like-text
                                      bing-dict-word-def-separator
                                      sounds-like-words)
                (bing-dict--message bing-dict--no-resul-text))))))
    (error (bing-dict--message bing-dict--no-resul-text))))

;;;###autoload
(defun bing-dict-brief (word)
  "Show the explanation of WORD from Bing in the echo area."
  (interactive
   (let* ((default (if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (let ((text (thing-at-point 'word)))
                       (if text (substring-no-properties text)))))
          (prompt (if (stringp default)
                      (format "Search Bing dict (default \"%s\"): " default)
                    "Search Bing dict: "))
          (string (read-string prompt nil 'bing-dict-history default)))
     (list string)))
  (save-match-data
    (url-retrieve (concat bing-dict--base-url
                          (url-hexify-string word))
                  'bing-dict-brief-cb
                  `(,(decode-coding-string word 'utf-8))
                  t
                  t)))

(provide 'bing-dict)
;;; bing-dict.el ends here
