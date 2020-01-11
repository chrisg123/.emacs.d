;;; my-line-numbers.el --- My line numbers
;;; Commentary:
;; Show line numbers when editing files.

;;; Code:

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(defvar linum-format)
(setq linum-format "%4d \u2502 ")
(provide 'my-line-numbers)

;;; my-line-numbers.el ends here
