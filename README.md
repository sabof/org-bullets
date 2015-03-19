<h2>org-bullets mode</h2>
Show org-mode bullets as UTF-8 characters.

![screenshot](https://github.com/sabof/org-bullets/raw/master/screenshot.png)

<h4>Installation</h4>
<h5>Initial Configuration</h5>
Copy the file somewhere in your load-path, then add to your .emacs:

    (require 'org-bullets)
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

select, do [M-x eval-region]. The *s will be replaced with utf-8 bullets next time you open an org file.  

<h5>Note about org-indent-mode</h5>
Org-bullet-mode can co-exist with org-indent-mode, but for correct loading, bullet mode should come before indent mode. E.g:
    
    (org-bullets-mode 1) ;comes first
    (org-indent-mode 1)  ;comes after
