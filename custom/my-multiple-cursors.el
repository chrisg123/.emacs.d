;;; my-multiple-cursors.el --- My line numbers
;;; Commentary:
;; Show line numbers when editing files.

;;; Code:

(require 'multiple-cursors)

(global-set-key (kbd "C-c i i") 'mc/edit-lines)
(global-set-key (kbd "C-c i >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c i <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c i ^") 'mc/mark-all-like-this)

(provide 'my-multiple-cursors)

;;; my-multiple-cursors.el ends here
