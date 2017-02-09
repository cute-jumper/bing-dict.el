;;; bing-dict-test.el --- Test for bing-dict

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

;;; Code:

(require 'bing-dict)

(defun bing-dict-brief-sync (word)
  (save-match-data
    (with-current-buffer (url-retrieve-synchronously
                          (concat bing-dict--base-url
                                  (url-hexify-string word)) t t)
      (bing-dict-brief-cb nil (decode-coding-string word 'utf-8))
      (substring-no-properties (current-message) 0))))

(defmacro bing-dict-sync-test-body (word &rest body)
  (declare (indent 1))
  `(save-match-data
     (with-current-buffer (url-retrieve-synchronously
                           (concat bing-dict--base-url
                                   (url-hexify-string ,word)) t t)
       ,@body)))

(ert-deftest bing-dict-brief ()
  (should
   (equal (bing-dict-brief-sync "good")
          "good [ɡʊd]: adv. 好 | n. 好处；好人；益处；善行 | adj. 有好处；好的；优质的；符合标准的 | Web 良好；很好；佳"))
  (should
   (equal (bing-dict-brief-sync "good job")
          "good job : Web 做得好；干得好；干的好"))
  (should
   (equal (bing-dict-brief-sync "That's very hard to do")
          "Machine translation: That's very hard to do --> 这是很难做到的"))
  (should
   (equal (bing-dict-brief-sync "pronuciation")
          "Sounds like: pronunciation 发音; pronunciations 发音; propitiation 和解; penetration 贯穿;透过;渗透力;突破;侵入;浸透;洞察;眼光;贯穿力;透视力;深入敌方的飞行;看破; procreation 生殖;生育;生产; "))
  (should
   (equal (bing-dict-brief-sync "abcdafgljasd")
          "No results"))
  (should
   (equal (bing-dict-brief-sync "好")
          "好 [hǎo] [hào] : adj. good; nice; fine; kind | v. love; like; be fond of; be liable to | adv. so as to; so that | n. a surname | Web OK; well; all right"))
  (should
   (equal (bing-dict-brief-sync "我能吞下玻璃而不伤身体")
          "Machine translation: 我能吞下玻璃而不伤身体 --> I swallowed the glass and not hurt"))
  (should
   (equal (bing-dict-brief-sync "我能吞下玻璃 而不伤身体")
          "Machine translation: 我能吞下玻璃 而不伤身体 --> I swallowed the glass and not hurt"))
  (should
   (equal
    (let ((bing-dict-show-thesaurus 'synonym))
      (bing-dict-brief-sync "difficult"))
    "difficult [ˈdɪfɪkəlt]: adj. 困难的；费力的；难做的；难解的 | 网络 艰难的；难以；不易相处的 | Synonym: hard,tricky,complicated,thorny,problematic"))
  (should
   (equal
    (let ((bing-dict-show-thesaurus 'antonym))
      (bing-dict-brief-sync "difficult"))
    "difficult [ˈdɪfɪkəlt]: adj. 困难的；费力的；难做的；难解的 | 网络 艰难的；难以；不易相处的 | Antonym: easy,simple,amenable"))
  (should
   (equal
    (let ((bing-dict-show-thesaurus 'both))
      (bing-dict-brief-sync "difficult"))
    "difficult [ˈdɪfɪkəlt]: adj. 困难的；费力的；难做的；难解的 | 网络 艰难的；难以；不易相处的 | Synonym: hard,tricky,complicated,thorny,problematic | Antonym: easy,simple,amenable")))
