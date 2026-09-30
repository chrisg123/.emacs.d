;;; my-markdown.el --- markdown settings
;;; Commentary:
;;; Code:
(require 'markdown-mode)

(setq markdown-fontify-code-blocks-natively t)

(defun my/insert-src-block-markdown (&optional language file)
  "Insert a Markdown fenced code block and leave point inside it.
Prompts for LANGUAGE when interactive.
With prefix arg, also prompts for FILE and inserts it."
  (interactive)
  (let ((col (current-column))
        (lang (or language (read-from-minibuffer "Fence language: ")))
        (file (if current-prefix-arg (read-file-name "Find file: ") nil)))
    ;; opening fence
    (insert (format "```%s" lang))
    (newline) (newline)

    ;; closing fence aligned to original column
    (move-to-column col t)
    (insert "```")
    (newline)

    ;; move back into the block
    (forward-line -2)
    (move-to-column col t)

    ;; optionally pull file contents into the block
    (when file
      (insert-file-contents file))))

(defun my-markdown-follow-file-with-line (url)
  "Open file:///...#L<line> links and jump to the line."
  (when (string-match
         "\\`file://\\([^#]+\\)#L\\([0-9]+\\)\\'" url)
    (let ((file (match-string 1 url))
          (line (string-to-number (match-string 2 url))))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))
      t)))                    ; Tell markdown-mode we handled it.

(add-hook 'markdown-follow-link-functions
          #'my-markdown-follow-file-with-line)

(provide 'my-markdown)
;;; my-markdown.el ends here
