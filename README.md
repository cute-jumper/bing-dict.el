# bing-dict.el
A **minimalists'** Emacs extension to show explanations from
(http://www.bing.com/dict).

## Setup

    (add-to-list 'load-path "/path/to/bing-dict.el")
    (require 'bing-dict)

## Usage
You can call `bing-dict-brief` to get the explanations of you query.
You should probably give this command a key binding:

    (global-set-key (kbd "C-c d") 'bing-dict-brief)

## Using Firefox's cookies
If you are using Firefox under Linux, you can search the word using your
Firefox's cookies. Why adding this functionality? Because if you've logged into
`bing.com` in your Firefox and you've joined the
[Bing Rewards](https://www.bing.com/rewards/dashboard) program, then your search
within Emacs also counts towards the number of total searches you've performed,
just like you're using Firefox to perform the Bing search.

To enable this, use the following setting:

    (setq bing-dict-use-firefox-cookies t)

If your Firefox cookie file is not something like
`~/.mozilla/firefox/*.default/cookies.sqlite`, then you should set the value of
`bing-dict-firefox-cookies-file`.

