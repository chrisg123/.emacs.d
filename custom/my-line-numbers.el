;;; my-line-numbers.el --- My line numbers
;;; Commentary:
;; Show line numbers when editing files.

;;; Code:

(require 'nlinum)
(global-nlinum-mode t)
(setq nlinum-format "%4d \u2502 ")
(provide 'my-line-numbers)

;;; my-line-numbers.el ends here
