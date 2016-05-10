# bing-dict.el
[![MELPA](http://melpa.org/packages/bing-dict-badge.svg)](http://melpa.org/#/bing-dict)
[![MELPA Stable](http://stable.melpa.org/packages/bing-dict-badge.svg)](http://stable.melpa.org/#/bing-dict)

A **minimalists'** Emacs extension to search http://www.bing.com/dict.
Support English to Chinese and Chinese to English.

极简主义者的 Emacs 必应词典。 支持中英互译。

## Setup

You can install via [melpa](http://melpa.org).

If installing this package manually:

    (add-to-list 'load-path "/path/to/bing-dict.el")
    (require 'bing-dict)

## Usage
You can call `bing-dict-brief` to get the explanations of you query. The results
will be shown in the echo area.

Here is the screenshot:

![bing-dict-screenshot](./screenshot.png)

You should probably give this command a key binding:

    (global-set-key (kbd "C-c d") 'bing-dict-brief)

## Customization
You can set the value of `bing-dict-add-to-kill-ring` to control whether the
result should be added to the `kill-ring` or not. By default, the value is
`nil`. If set to `t`, the result will be added to the `kill-ring` and you are
able to use `C-y` to paste the result.

## As for More Features...
This extension aims for a quick search for a word. I don't plan to parse all the
sections of the search results. If you want to view the complete results of your
query word, I suggest using external browser to do this. The following code
could partly achieve the goal:

    (browse-url
     (concat "http://www.bing.com/dict/search?q="
           (url-hexify-string
            (read-string "Query: "))))
