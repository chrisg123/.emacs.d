;;; my-lsp.el --- Lsp customization
;;; Commentary:

;;; Code:
(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-ui-doc)
;;(lsp-ui-flycheck-add-mode 'kotlin-mode)
(add-to-list 'flycheck-checkers 'lsp-ui)

(defun lsp-mode-keybinding()
  "Setup keybinding for lsp-mode."
  (local-set-key (kbd "M-.") 'lsp-find-definition)
  (local-set-key (kbd "M-?") 'lsp-find-references)
  (local-set-key (kbd "C-c C-f") 'lsp-format-region)
  (local-set-key (kbd "C-c RET") 'lsp-ui-doc-show)
  (local-set-key (kbd "C-c M-RET") 'lsp-ui-doc-show-separate-buf))

(defun lsp-mode-settings()
  "Custom lsp-mode settings."
  (setq lsp-log-io t)
  (setq lsp-ui-doc-max-width 80)
  )

(add-hook 'lsp-mode-hook
	  (lambda()
	    (lsp-mode-keybinding)
	    (lsp-mode-settings)
	    (lsp-ui-mode)
	    (setq lsp-headerline-breadcrumb-enable nil)))

(add-hook 'kotlin-mode-hook
	  (lambda()
	    (lsp)
	    (flycheck-mode)
	    ))

(defun lsp-ui-doc--inline-wrapped-line (string)
  "Wraps a line of text (STRING) for inline display."
  (cond ((string-empty-p string) "")
        (t (let ((wrapped string)
                 (fill-column (- (window-width) 5)))
             (with-temp-buffer
               (insert string)
               (end-of-line)
               (when (> (current-column) fill-column)
                 (fill-region 0 (point) 'left t))
               (setq wrapped (buffer-string)))
             wrapped))))

(defun lsp-ui-doc-show-separate-buf ()
  "Trigger display information buffer."
  (interactive)
  (lsp-ui-doc-show-separate-buf--callback
   (lsp-request "textDocument/hover" (lsp--text-document-position-params))
   (or (bounds-of-thing-at-point 'symbol) (cons (point) (1+ (point))))
   (current-buffer)))


(lsp-defun lsp-ui-doc-show-separate-buf--callback ((hover &as &Hover? :contents) bounds buffer)
  "Process the received documentation.
HOVER is the doc returned by the LS.
BOUNDS are points of the symbol that have been requested.
BUFFER is the buffer where the request has been made."
  (let ((bounds (or (lsp-ui-doc--extract-bounds hover) bounds))
        (bufname "*lsp-ui-doc*"))
    (if (and hover
             (>= (point) (car bounds))
             (<= (point) (cdr bounds))
             (eq buffer (current-buffer)))
        (progn
          (when (get-buffer bufname) (kill-buffer bufname))
          (lsp-ui-doc-hide)
          (setq lsp-ui-doc--bounds bounds)
          (let ((string (-some->> contents
                          lsp-ui-doc--extract
                          (replace-regexp-in-string "\r" "")
                          (replace-regexp-in-string " " " "))))

            (with-current-buffer (get-buffer-create bufname)
              (insert string)
              (read-only-mode)
              (goto-char (point-min)))
            (switch-to-buffer-other-window bufname)
            ))
      (lsp-ui-doc--hide-frame))))

(provide 'my-lsp)

;;; my-lsp.el ends here
