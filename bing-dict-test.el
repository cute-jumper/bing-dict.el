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
  (bing-dict-brief word t)
  (substring-no-properties (current-message) 0))

(defmacro bing-dict-sync-test-body (word &rest body)
  (declare (indent 1))
  `(save-match-data
     (with-current-buffer (url-retrieve-synchronously
                           (concat bing-dict--base-url
                                   (url-hexify-string ,word)) t t)
       ,@body)))

(ert-deftest bing-dict-brief ()
  (setq bing-dict-pronunciation-style 'us
        bing-dict-show-thesaurus nil
        bing-dict-cache-auto-save nil
        bing-dict--cache nil)
  (should
   (equal (bing-dict-brief-sync "good")
          "good [ɡʊd]: adv. 好 | n. 好处；好人；益处；善行 | adj. 有好处；好的；优质的；符合标准的 | 网络 良好；很好；佳"))
  (should
   (equal (bing-dict-brief-sync "good job")
          "good job : 网络 做得好；干得好；干的好"))
  (should
   (equal (bing-dict-brief-sync "That's very hard to do")
          "Machine translation: That’s very hard to do -> 这是很难做到的"))
  (should
   (equal (bing-dict-brief-sync "pronuciation")
          "Sounds like: pronunciation 发音; pronunciations 发音; propitiation 和解; penetration 贯穿;透过;渗透力;突破;侵入;浸透;洞察;眼光;贯穿力;透视力;深入敌方的飞行;看破; procreation 生殖;生育;生产; "))
  (should
   (equal (bing-dict-brief-sync "abcdafgljasd")
          "No results"))
  (should
   (equal (bing-dict-brief-sync "好")
          "好 [hǎo] [hào] : adj. good; nice; fine; kind | v. love; like; be fond of; be liable to | n. a surname | adv. so as to; so that | 网络 OK; well; all right"))
  (should
   (equal (bing-dict-brief-sync "我能吞下玻璃而不伤身体")
          "Machine translation: 我能吞下玻璃而不伤身体 -> I swallowed the glass and not hurt"))
  (should
   (equal (bing-dict-brief-sync "我能吞下玻璃 而不伤身体")
          "Machine translation: 我能吞下玻璃 而不伤身体 -> I swallowed the glass and not hurt"))
  (should
   (equal
    (let (bing-dict-pronunciation-style)
      (bing-dict-brief-sync "center"))
    "center [ˈsentə(r)]: n. 中心；中央；核心；圆心 | v. 居中；把…置于中部；有中心；做中锋 | adj. 中点的 | 网络 居中对齐；中心点"))
  (should
   (equal
    (let ((bing-dict-show-thesaurus 'synonym))
      (bing-dict-brief-sync "center"))
    "center [ˈsentər]: n. 中心；中央；核心；圆心 | v. 居中；把…置于中部；有中心；做中锋 | adj. 中点的 | 网络 居中对齐；中心点 | Synonym n. axis,cluster,complex,downtown v. adjust,arrange,balance,concentrate on,focus on"))
  (should
   (equal
    (let ((bing-dict-show-thesaurus 'antonym))
      (bing-dict-brief-sync "center"))
    "center [ˈsentər]: n. 中心；中央；核心；圆心 | v. 居中；把…置于中部；有中心；做中锋 | adj. 中点的 | 网络 居中对齐；中心点 | Antonym n. coating,edge,extreme,periphery v. ignore"))
  (should
   (equal
    (let ((bing-dict-show-thesaurus 'both))
      (bing-dict-brief-sync "center"))
    "center [ˈsentər]: n. 中心；中央；核心；圆心 | v. 居中；把…置于中部；有中心；做中锋 | adj. 中点的 | 网络 居中对齐；中心点 | Synonym n. axis,cluster,complex,downtown v. adjust,arrange,balance,concentrate on,focus on | Antonym n. coating,edge,extreme,periphery v. ignore")))

(ert-deftest bing-dict-cache ()
  (should
   (let ((bing-dict--cache '(("test" mock-result . mock-time)
                             ("plain" mock-result . mock-time)
                             ("a" mock-result . mock-time)
                             ("is" mock-result . mock-time)
                             ("This" mock-result . mock-time)))
         (bing-dict-cache-auto-save t)
         (bing-dict-cache-limit 3))
     (equal (bing-dict--cache-overflow-p) t)))

  (should
   (let ((bing-dict--cache nil)
         (bing-dict-cache-auto-save t)
         (bing-dict-cache-limit 4))
     (bing-dict-brief-sync "This")
     (let ((saved-time (cdr (assoc-default "This" bing-dict--cache))))
       (bing-dict-brief-sync "This")
       (> (cdr (assoc-default "This" bing-dict--cache)) saved-time))))

  (should
   (let ((bing-dict--cache nil)
         (bing-dict-cache-auto-save t)
         (bing-dict-cache-limit 4))
     (bing-dict-brief-sync "This")
     (bing-dict-brief-sync "is")
     (bing-dict-brief-sync "a")
     (bing-dict-brief-sync "plain")
     (bing-dict-brief-sync "test")
     (equal (length bing-dict--cache) 4))))
