# bing-dict.el
A **minimalists'** Emacs extension to search (http://www.bing.com/dict). 
Support English to Chinese and Chinese to English.

## Setup

    (add-to-list 'load-path "/path/to/bing-dict.el")
    (require 'bing-dict)

## Usage
You can call `bing-dict-brief` to get the explanations of you query. The results
will be shown in the minibuffer.

Here is the screenshot:

![bing-dict-screenshot](./screenshot.png)

You should probably give this command a key binding:

    (global-set-key (kbd "C-c d") 'bing-dict-brief)

## As for More Features...
This extension aims for a quick search for a word. I don't plan to parse all the
sections of the search results. If you want to view the complete results of your
query word, I suggest using external browser to do this. The following code
could partly achieve the goal:

    (browse-url
     (concat "http://www.bing.com/dict/search?q="
           (url-hexify-string
            (read-string "Query: "))))
