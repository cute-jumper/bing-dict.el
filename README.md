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

## Using Firefox's Cookies Under Linux
Note this feature is only tested for Firefox 37 under Linux.

If you are using Firefox under Linux, you can search the word using your
Firefox's cookies. Why adding this feature? Because if you've logged into
`bing.com` in your Firefox and you've joined the
[Bing Rewards](https://www.bing.com/rewards/dashboard) program, then your search
within Emacs also counts towards the number of total searches you've performed,
just like you're using Firefox to perform the Bing search.

To enable this, use the following setting:

    (setq bing-dict-use-firefox-cookies t)

If your Firefox cookie file is not something like
`~/.mozilla/firefox/*.default/cookies.sqlite`, then you should also set the
value of `bing-dict-firefox-cookies-file`.

## As for More Features...
This extension aims for a quick search for a word. I don't plan to parse all the
sections of the search results. If you want to view the complete results of your
query word, I suggest using external browser to do this. The following code
could partly achieve the goal:

    (browse-url
     (concat "http://www.bing.com/dict/search?q="
           (url-hexify-string
            (read-string "Query: "))))
