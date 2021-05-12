;;; emacs-keys.el --- Emacs custom key bindings
;;; Commentary:
;;; Code:
(define-key global-map (kbd "C-c o") 'insert-org-mode-header)

(define-key global-map (kbd "C-c s") 'insert-source-block)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "C-c w") 'whitespace-cleanup)

(defvar xah-math-input-keymap)
(add-hook
 'xah-math-input-mode-hook
 (lambda()
   (define-key xah-math-input-keymap
     (kbd "<f8>") 'xah-math-input-change-to-symbol)))


(provide 'emacs-keys)

;;; emacs-keys.el ends here
