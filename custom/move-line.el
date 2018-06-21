;; https://www.emacswiki.org/emacs/MoveLine
;; This is a move-line function (by Joe Smith) that was discussed on the
;; gnu-help mailing list back in 2008. This function allows the current line
;; to be transposed up and down. This is useful for re-arranging lists in Emacs.
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column)) (start nil) (end nil) (line-text nil))
    (beginning-of-line) (setq start (point))
    (end-of-line) (forward-char) (setq end (point))
  (setq line-text (delete-and-extract-region start end))
    (forward-line n) (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1) (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(provide 'move-line)
