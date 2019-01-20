;;; my-projectile.el --- Helm customization
;;; Commentary:

;;; Code:
(require 'projectile)

;; Below keymap prefixes are just suggestions.
;; See https://docs.projectile.mx/en/latest/installation/
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(projectile-mode +1)

(provide 'my-projectile)
;;; my-projectile.el ends here
