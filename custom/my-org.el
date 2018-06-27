;;; my-org.el --- Emacs auto save settings
;;; Commentary:
;;; Code:

(defvar org-log-done)
(setq org-log-done 'time)

(defvar org-hide-emphasis-markers)
(setq org-hide-emphasis-markers t)

(defun insert-source-block()
  "Insert source code declaration block."
  (interactive)
  (let ((col (current-column))(input (read-from-minibuffer "language: ")))
    (insert (format "#+BEGIN_SRC %s" input))(newline)(newline)
    (move-to-column col t)(insert "#+END_SRC")(newline)
    (forward-line -2)(move-to-column col t)))

(defvar src-block-overlays)

(defun hide-source-block-delimeters()
  "Hide source block delimiters."
  (interactive)
  (save-excursion
       (goto-char (point-max))
       (setq src-block-overlays (list))
       (while (re-search-backward "#\\+BEGIN_SRC\\|#\\+END_SRC" nil t)
	 (let ((ov-src-block-delim
		(make-overlay (line-beginning-position) (1+ (line-end-position)))))
	   (overlay-put ov-src-block-delim 'src-block-delim t)
	   (overlay-put ov-src-block-delim 'invisible t)
	   (push ov-src-block-delim src-block-overlays)))))

(defun unhide-source-block-delimiters()
  "Unhide source block delimiters."
  (interactive)
  (delete-overlays src-block-overlays))

(defun delete-overlays (ovs)
  "Delete OVS."
  (if (overlayp (car ovs)) (delete-overlay (car ovs)))
  (if ovs (delete-overlays (cdr ovs))))

(defun insert-org-mode-header()
  "Insert org mode declaration header."
  (interactive)
  (insert "# -*- mode: org -*-")(newline)
  (let ((input (read-from-minibuffer "Title: ")))
    (insert (format "#+TITLE: %s" input)))(newline)
    (insert "#+DATE:")(newline)
    (insert "#+OPTIONS: toc:nil") ; no table of contents
  (end-of-line)(newline)(newline)
  (save-buffer t)
  (revert-buffer :ignore-auto :nonconfirm)
  )

(provide 'my-org)

;;; my-org.el ends here
