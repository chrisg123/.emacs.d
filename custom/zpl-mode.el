;;; zpl-mode.el --- Zebra Programming Language major mode

;;; Commentary:

;;; Code:

(defvar zpl-mode-hook nil)


(defvar zpl-mode-map
  (let ((map (make-sparse-keymap)))
    map)
)

(add-to-list 'auto-mode-alist '("\\.zpl\\'" . zpl-mode))

(defconst zpl-font-lock-keywords-1
  (list
   '("\\([\\^|\\~][[:alpha:]]\\{2\\}\\)" . font-lock-builtin-face)
   )
  "Basic highlighting expressions for ZPL mode.")

(defconst zpl-font-lock-keywords-2
  (append zpl-font-lock-keywords-1
  (list
   '("\\(\\^A\\)" . font-lock-builtin-face)
   ))
  "Specific highlighting expressions for ZPL mode.")

(defconst zpl-font-lock-keywords zpl-font-lock-keywords-2
  "Default highlighting in ZPL mode.")

(defun zpl-indent-line ()
  "Indent current line as ZPL code."
  (interactive)
  (beginning-of-line)
  (while (looking-at " ")
    (delete-char 1)
    )
)

(defvar zpl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?^ "w" st)
    st)
    "Syntax table for zpl-mode.")

(defun zpl-mode ()
  "Major mode for editing Zebra Programming Language files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table zpl-mode-syntax-table)
  (use-local-map zpl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(zpl-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'zpl-inden-line)
  (set major-mode 'zpl-mode)
  (setq mode-name "ZPL")
  (run-hooks 'zpl-mode-hook)
)
(provide 'zpl-mode)
;;; zpl-mode.el ends here
