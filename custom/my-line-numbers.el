;;; my-line-numbers.el --- My line numbers
;;; Commentary:
;; Show line numbers when editing files.

;;; Code:
(require 'display-line-numbers)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(defvar linum-format)
(setq linum-format "%4d \u2502 ")

(setq display-line-numbers-width-start 5)

(setq display-line-numbers-type 'relative)

(provide 'my-line-numbers)

;;; my-line-numbers.el ends here
