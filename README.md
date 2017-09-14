<h2>org-bullets mode</h2>
Show org-mode bullets as UTF-8 characters.

![screenshot](https://github.com/sabof/org-bullets/raw/master/screenshot.png)

<h4>Installation</h4>
Copy the file somewhere in your load-path, then add to your .emacs:

    (require 'org-bullets)
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

select, do [M-x eval-region]. The *s will be replaced with utf-8 bullets next time you open an org file


#### Hiding leading stars

org-bullets used to hide the leading stars and only show the last one (as a bullet). This feature was removed since there already is a feature in org-mode which does exactly that (read about it [here](https://www.gnu.org/software/emacs/manual/html_node/org/Clean-view.html) and since it didn't work well for emacs running in a terminal. For enabling it add this to your .emacs:

    (setq org-hide-leading-stars t)
