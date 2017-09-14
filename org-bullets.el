;;; org-bullets.el --- Show bullets in org-mode as UTF-8 characters
;;; Version: 0.2.4
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

(defgroup org-bullets nil
  "Display bullets as UTF-8 characters"
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

(defcustom org-bullets-face-name nil
  "Face to override `org-mode' bullets face.

If set to a name of a face, that face will be used.  When nil, do
not change the face used."
  :group 'org-bullets
  :type '(choice (const :tag "Off" nil)
                 (face :tag "Face")))

(defcustom org-bullets-compose-leading-stars
  (and org-hide-leading-stars hide)
  "Replace leading stars with the bullet character.

This is different from `org-hide-leading-stars' in that it
replace the printed character instead of changing the face."
  :group 'org-bullets
  :type '(choice (const :tag "Keep leading stars" nil)
                 (const :tag "Hide leading stars" hide)
                 (const :tag "Use current level character" level)
                 (string :tag "Use custom character(s)")))


(defvar org-bullets-bullet-map
  '(keymap
    (mouse-1 . org-cycle)
    (mouse-2 . (lambda (e)
                 (interactive "e")
                 (mouse-set-point e)
                 (org-cycle))))
  "Mouse events for bullets.
Should this be undesirable, one can remove them with:

\(setcdr org-bullets-bullet-map nil)")

(defun org-bullets-level-char (level)
  "Return the bullet character for LEVEL.

The bullet character is periodic in that if LEVEL is greater than
the `org-bullets-bullet-list' lenght, the modulo is used."
  (string-to-char
   (nth (mod (1- level)
             (length org-bullets-bullet-list))
        org-bullets-bullet-list)))

(defun org-bullets--char-series (string level)
  "Private.

Get the character in STRING at position LEVEL.

If LEVEL is greater than the STRING series length, use the reminder."
  (aref string (mod level (length string))))

;;;###autoload
(define-minor-mode org-bullets-mode
  "UTF8 Bullets for org-mode."
  nil nil nil
  (let* ((keyword
          `(("^\\*+ "
             (0 (let* ((level (- (match-end 0) (match-beginning 0) 1))
                       (is-inline-task
                        (and (boundp 'org-inlinetask-min-level)
                             (>= level org-inlinetask-min-level)))
                       (series))
                  (compose-region (- (match-end 0) 2)
                                  (- (match-end 0) 1)
                                  (org-bullets-level-char level))
                  (when is-inline-task
                    (compose-region (- (match-end 0) 3)
                                    (- (match-end 0) 2)
                                    (org-bullets-level-char level)))
                  (when (facep org-bullets-face-name)
                    (put-text-property (- (match-end 0)
                                          (if is-inline-task 3 2))
                                       (- (match-end 0) 1)
                                       'face
                                       org-bullets-face-name))

                  (pcase org-bullets-compose-leading-stars
                    ((pred stringp) (setq series org-bullets-compose-leading-stars))
                    ('hide (setq series " "))
                    ('level (setq series (apply #'concat org-bullets-bullet-list))))

                  (if series
                      (dotimes (pos (1- level))
                        (compose-region (+ (match-beginning 0) pos)
                                        (+ (match-beginning 0) pos 1)
                                        (org-bullets--char-series series pos))))
                  (add-text-properties (match-beginning 0)
                                       (match-end 0)
                                       (list 'keymap org-bullets-bullet-map
                                             'mouse-face 'highlight
                                             'help-echo "mouse-2: visibility cycling for Org mode"))
                  nil))))))
    (if org-bullets-mode
        (progn
          (font-lock-add-keywords nil keyword)
          (font-lock-flush))
      (save-excursion
        (goto-char (point-min))
        (font-lock-remove-keywords nil keyword)
        (while (re-search-forward "^\\*+ " nil t)
          (decompose-region (match-beginning 0) (match-end 0)))
        (font-lock-flush)))))

(provide 'org-bullets)

;;; org-bullets.el ends here
