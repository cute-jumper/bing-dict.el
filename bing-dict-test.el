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

(bing-dict-brief "good")
(bing-dict-brief "good job")
(bing-dict-brief "That's very hard to do")
(bing-dict-brief "pronuciation")
(bing-dict-brief "abcdafgljasd")
(bing-dict-brief "好")
(bing-dict-brief "我能吞下玻璃而不伤身体")
(bing-dict-brief "我能吞下玻璃 而不伤身体")
