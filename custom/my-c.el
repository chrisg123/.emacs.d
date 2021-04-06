;;; my-c.el --- C customization
;;; Commentary:

;;; Code:
(defvar c-mode-base-map)
(add-hook
 'c-mode-common-hook
 (lambda()
   (define-key c-mode-base-map
     (kbd "M-.") 'semantic-ia-fast-jump)))

(provide 'my-c)
;;; my-c.el ends here
