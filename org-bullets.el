;;; org-bullets.el --- Show bullets in org-mode as UTF-8 characters
;;; Version: 0.1
;;; Author: sabof
;;; URL: https://github.com/sabof/org-bullets

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The project is hosted at https://github.com/sabof/org-bullets
;; The latest version, and all the relevant information can be found there.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup org-bullets nil
  "Use different background for even and odd lines."
  :group 'org-appearance)

;; A nice collection of unicode bullets:
;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
(defcustom org-bullets-bullet-list
  '(;;; Large
    "◉"
    "○"
    "✸"
    "✿"
    ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
    ;; ► • ★ ▸
    )
  "This variable contains the list of bullets.
It can contain any number of symbols, which will be repeated."
  :group 'org-bullets
  :type '(repeat (string :tag "Bullet character")))

(defvar org-bullet-overlays nil)
(make-variable-buffer-local 'org-bullet-overlays)

(defvar org-bullets-changes nil)
(make-variable-buffer-local 'org-bullets-changes)

(defun org-bullets-match-length ()
  (- (match-end 0) (match-beginning 0)))

(defcustom org-bullets-face-name
  nil
  "This variable allows the face to be used for Org-mode bullets
to be overridden. If nil then the standard Org-mode face for the
current heading's level will be used; set this to a string to
override this face."
  :group 'org-bullets
  :type 'string)

(defun org-bullets-make-face-name ()
  (or (get-char-property (point) 'read-face-name)
      (get-char-property (point) 'face)))

(defun org-bullets-make-star (bullet-string counter)
  (let* ((map '(keymap
                (mouse-1 . org-cycle)
                (mouse-2 . (lambda (e)
                             (interactive "e")
                             (mouse-set-point e)
                             (org-cycle)))))
         (face (or org-bullets-face-name
		   (org-bullets-make-face-name)))
         (overlay (make-overlay (point)
                                (1+ (point)))))
    (overlay-put overlay 'display
                 (if (zerop counter)
                     (propertize bullet-string
                                 'face face
                                 'local-map map)
                     (propertize " "
                                 'local-map map)))
    (overlay-put overlay 'is-bullet t)
    (push overlay org-bullet-overlays)))

(defun org-bullets-clear ()
  (mapc 'delete-overlay org-bullet-overlays)
  (setq org-bullet-overlays nil))

(defun* org-bullets-redraw (&optional (beginning (point-min)) (end (point-max)))
  (save-excursion
    (save-match-data
      (mapc 'delete-overlay
            (remove-if-not
             (lambda (overlay) (overlay-get overlay 'is-bullet))
             (overlays-in beginning end)))
      (goto-char beginning)
      (while (and (re-search-forward "^\\*+" nil t)
                  (<= (point) end))
        (let* ((bullet-string (nth (mod (1- (org-bullets-match-length))
                                        (list-length org-bullets-bullet-list))
                                   org-bullets-bullet-list)))
          (goto-char (match-beginning 0))
          (if (save-match-data (looking-at "^\\*+ "))
              (let ((counter (1- (org-bullets-match-length))))
                (while (looking-at "[* ]")
                  (org-bullets-make-star bullet-string counter)
                  (forward-char)
                  (decf counter)))
              (goto-char (match-end 0)))
          )))))

(defun org-bullets-notify-change (&rest args)
  (push args org-bullets-changes))

(defun* org-bullets-post-command-hook (&rest ignore)
  (unless org-bullets-changes
    (return-from org-bullets-post-command-hook))
  (let ((min (reduce 'min org-bullets-changes :key 'first))
        (max (reduce 'max org-bullets-changes :key 'second)))
    (org-bullets-redraw (save-excursion
                          (goto-char min)
                          (line-beginning-position))
                        (save-excursion
                          (goto-char max)
                          (forward-line)
                          (line-end-position))))
  (setq org-bullets-changes nil))

;;; Interface

;;;###autoload
(define-minor-mode org-bullets-mode
    "UTF8 Bullets for org-mode"
  nil nil nil
  (if org-bullets-mode
      (progn
        (add-hook 'after-change-functions 'org-bullets-notify-change nil t)
        (add-hook 'post-command-hook 'org-bullets-post-command-hook nil t)
        (org-bullets-redraw))
      (remove-hook 'after-change-functions 'org-bullets-notify-change t)
      (remove-hook 'post-command-hook 'org-bullets-post-command-hook t)
      (mapc 'delete-overlay org-bullet-overlays)))

(provide 'org-bullets)

;;; org-bullets.el ends here
