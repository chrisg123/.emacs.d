;;; my-helm.el --- Helm customization
;;; Commentary:

;;; Code:
(require 'helm-mode)
(helm-mode 1)

(defvar helm-map)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(add-to-list
 'helm-completing-read-handlers-alist '(org-insert-source-code-block . nil))



(provide 'my-helm)
;;; my-helm.el ends here
