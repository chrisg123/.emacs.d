;;; emacs-keys.el --- Emacs custom key bindings
;;; Commentary:
;;; Code:
(define-key global-map (kbd "C-c o") 'insert-org-mode-header)

(define-key global-map (kbd "C-c s") 'insert-source-block)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(provide 'emacs-keys)

;;; emacs-keys.el ends here
