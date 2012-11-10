;;; org-bullets.el --- Show bullets in org-mode as UTF-8 characters
;;; Version: 0.1
;;; Author: sabof

;;; Commentary:

;; The project is hosted at https://github.com/sabof/org-bullets
;; The latest version, and all the relevant information can be found there.

;;; Code:

(require 'cl)

(defgroup org-bullets nil
  "Use different background for even and odd lines."
  :group 'org-appearance)

;; A nice collection of unicode bullets:
;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
(defcustom org-bullets-bullet-list
  '(
    ;;; Large
    ;; ●
    "◉"
    ;; ♥
    "○"
    "✸"
    ;; "◇"
    "✿"
    ;; ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀
    ;; "◆"
    ;; ◖
    ;; "▶"
    ;;; Small
    ;; ►
    ;; "•"
    ;; "★"
    ;; "▸"
    )
  "This variable contains the list of bullets.
It can contain any number of symbols, which will be repeated."
  :group 'org-bullets
  :type '(repeat (string)))

(defvar org-bullet-overlays nil)
(setq-default org-bullet-overlays nil)
(make-variable-buffer-local 'org-bullet-overlays)

(defvar org-bullets-has-changed nil)
(make-variable-buffer-local 'org-bullets-has-changed)

(defun org-bullets-match-length ()
  (- (match-end 0) (match-beginning 0)))

(defun* org-bullets-make-star (bullet-string counter)
  (let* ((map '(keymap
                (mouse-1 . org-cycle)
                (mouse-2 . (lambda (e)
                             (interactive "e")
                             (mouse-set-point e)
                             (org-cycle)))))
         (face (save-excursion
                 (save-match-data
                   (beginning-of-line)
                   (looking-at "\\*+")
                   (intern (concat "org-level-"
                                   (int-to-string
                                    (1+ (mod (1- (org-bullets-match-length))
                                             8))))))))
         (overlay (make-overlay (point)
                                (1+ (point)))))
    (overlay-put overlay 'display
                 (if (zerop counter)
                     (propertize bullet-string
                                 'face face
                                 'local-map map)
                     (propertize " "
                                 'local-map map)))
    (push overlay org-bullet-overlays)))

(defun org-bullets-clear ()
  (mapc 'delete-overlay org-bullet-overlays)
  (setq org-bullet-overlays nil))

(defun org-bullets-redraw (&rest ignore)
  (save-excursion
    (save-match-data
      (org-bullets-clear)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+" nil t)
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

(defun org-bullets-notify-change (&rest ignore)
  (setq org-bullets-has-changed t))

(defun* org-bullets-post-command-hook (&rest ignore)
  (unless org-bullets-has-changed
    (return-from org-bullets-post-command-hook))
  (when (and (eq last-command 'self-insert-command)
             (not (equal last-input-event ?\* )))
    (return-from org-bullets-post-command-hook))
  (org-bullets-redraw)
  (setq org-bullets-has-changed nil))

(defun* org-bullets-enable ()
  (add-hook 'after-change-functions 'org-bullets-notify-change nil t)
  (add-hook 'post-command-hook 'org-bullets-post-command-hook nil t)
  (org-bullets-redraw)
  )

(defun* org-bullets-disable ()
  (remove-hook 'after-change-functions 'org-bullets-notify-change t)
  (remove-hook 'post-command-hook 'org-bullets-post-command-hook t)
  (mapc 'delete-overlay org-bullet-overlays)
  nil)

;;; Interface

(define-minor-mode org-bullets-mode
    "UTF8 Bullets for org-mode"
  nil nil nil
  (if org-bullets-mode
      (org-bullets-enable)
      (org-bullets-disable)))

(provide 'org-bullets)

;;; org-bullets.el ends here