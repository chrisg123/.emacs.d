;;; my-helm.el --- Helm customization
;;; Commentary:

;;; Code:
(require 'helm-config)
(helm-mode 1)

(defvar helm-map)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(provide 'my-helm)
;;; my-helm.el ends here
