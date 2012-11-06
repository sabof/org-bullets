(require 'cl)
(defvar org-bullets-dots nil)
(setq-default org-bullets-dots nil)
(make-variable-buffer-local 'org-bullets-dots)
(defvar org-bullets-has-changed nil)
(make-variable-buffer-local 'org-bullets-has-changed)
;; A collection of unicode bullets
;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
(defvar org-bullets-bullet-list
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
    ))

(defun match-length ()
  (- (match-end 0) (match-beginning 0)))

(defun org-bullets-cycle (event)
  (interactive "E")
  (mouse-set-point event)
  (org-cycle))

(defun* org-bullets-make-star (bullet-string)
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
                                    (1+ (mod (1- (match-length))
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
    (push overlay org-bullets-dots)))

(defun org-bullets-clear ()
  (mapc 'delete-overlay org-bullets-dots)
  (setq org-bullets-dots nil))

(defun org-bullets (&rest ignore)
  (save-excursion
    (save-match-data
      (org-bullets-clear)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+" nil t)
        (let* ((bullet-string (nth (mod (1- (match-length))
                                        (list-length org-bullets-bullet-list))
                                   org-bullets-bullet-list)))
          (goto-char (match-beginning 0))
          (if (save-match-data (looking-at "^\\*+ "))
              (let ((counter (1- (match-length))))
                (while (looking-at "[* ]")
                  (org-bullets-make-star bullet-string)
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
  (org-bullets)
  (setq org-bullets-has-changed nil))

(defun* org-bullets-enable ()
  (add-hook 'after-change-functions 'org-bullets-notify-change nil t)
  (add-hook 'post-command-hook 'org-bullets-post-command-hook nil t)
  (org-bullets)
  )

(defun* org-bullets-disable ()
  (jit-lock-unregister 'org-bullets)
  (remove-hook 'after-change-functions 'org-bullets-notify-change t)
  (remove-hook 'post-command-hook 'org-bullets-post-command-hook t)
  (mapc 'delete-overlay org-bullets-dots)
  nil)

(define-minor-mode org-bullets-mode
    "UTF8 Bullets for org-mode"
  nil nil nil
  (if org-bullets-mode
      (org-bullets-enable)
      (org-bullets-disable)))

(provide 'org-bullets)