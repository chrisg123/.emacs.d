;;; emacs-keys.el --- Emacs custom key bindings
;;; Commentary:
;;; Code:
(define-key global-map (kbd "C-c o") 'insert-org-mode-header)

(define-key global-map (kbd "C-c s") 'insert-source-block)

(provide 'emacs-keys)

;;; emacs-keys.el ends here
