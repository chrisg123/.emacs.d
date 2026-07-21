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

(provide 'my-markdown)
;;; my-markdown.el ends here
