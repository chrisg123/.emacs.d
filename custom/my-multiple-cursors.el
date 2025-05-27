;;; my-multiple-cursors.el --- My line numbers
;;; Commentary:
;; Show line numbers when editing files.

;;; Code:
(require 'sgml-mode)
(require 'multiple-cursors)

(global-set-key (kbd "C-c i i") 'mc/edit-lines)
(global-set-key (kbd "C-c i >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c i <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c i ^") 'mc/mark-all-like-this)

;; Mark mc--insert-number-and-increase as run-once to prevent duplicate numbering
(with-eval-after-load 'multiple-cursors
  (add-to-list 'mc/cmds-to-run-once 'mc--insert-number-and-increase))

(provide 'my-multiple-cursors)

;;; my-multiple-cursors.el ends here

