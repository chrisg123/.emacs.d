;;; my-js.el --- Js customization
;;; Commentary:

;;; Code:
(defun my-js-mode-keys ()
  "Set custom keybindings for JavaScript mode."
  (local-set-key (kbd "C-c C-e e") 'end-of-defun)
  (local-set-key (kbd "C-c C-e b") 'beginning-of-defun))

(add-hook 'js-mode-hook 'my-js-mode-keys)
;;(add-hook 'js-ts-mode-hook 'my-js-mode-keys) ;; If using Tree-sitter

(provide 'my-js)

;;; my-js.el ends here
