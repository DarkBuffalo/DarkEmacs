  (use-package engine-mode
    :config
    (defengine google "https://google.com/search?q=%s" :keybinding "g"
               :docstring "Search Google.")
    (defengine google-images "https://www.google.com/search?tbm=isch&q=%s" :keybinding "i"
               :docstring "Search Google Images")
    (defengine google-maps "http://maps.google.com/maps?q=%s" :keybinding "m"
               :docstring "Search Google Maps.")
    (defengine duckduckgo "https://duckduckgo.com/?q=%s" :keybinding "d"
               :docstring "Search DuckDuckGo.")
    (defengine jw "https://wol.jw.org/fr/wol/s/r30/lp-f?p=par&r=occ&st=a?q=%" :keybinding "j"
               :docstring "Search jw.org.")
    (defengine qwant "https://www.qwant.com/?q=%s" :keybinding "q"
               :docstring "Search Qwant.")
    (defengine wikipedia "https://en.wikipedia.org/wiki/Special:Search?search=%s" :keybinding "w"
               :docstring "Search Wikipedia.")
    (defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s" :keybinding "y"
               :docstring "Search YouTube.")
    (defengine github "https://github.com/search?ref=simplesearch&q=%s" :keybinding "h"
               :docstring "Search GitHub.")
    (defengine melpa "https://melpa.org/#/?q=%s" :keybinding "p"
               :docstring "Search the Milkypostman's Emacs Lisp Package Archive.")
    (defengine stack-overflow "https://stackoverflow.com/search?q=%s" :keybinding "s"
               :docstring "Search Stack Overflow.")
    (defengine wolfram-alpha "http://www.wolframalpha.com/input/?i=%s" :keybinding "a"
               :docstring "Search Wolfram Alpha.")
    (defengine rfcs "http://pretty-rfc.herokuapp.com/search?q=%s" :keybinding "r"
               :docstring "Search RFC documents.")
    (defengine ctan "http://www.ctan.org/search/?x=1&PORTAL=on&phrase=%s" :keybinding "c"
               :docstring "Search the Comprehensive TeX Archive Network")
    (defengine project-gutenberg "http://www.gutenberg.org/ebooks/search/?query=%s" :keybinding "p"
               :docstring "Search Project Gutenberg.")
    (engine/set-keymap-prefix (kbd "C-x /"))
    (setq engine/browser-function 'browse-url-firefox)
    :init
    (engine-mode t))
