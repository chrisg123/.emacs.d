;;; my-kotlin.el --- Kotlin customization
;;; Commentary:

;;; Code:

(add-hook
 'kotlin-mode-hook
 (lambda() (setq-default kotlin-tab-width 4)))

(provide 'my-kotlin)

;;; my-kotlin.el ends here
