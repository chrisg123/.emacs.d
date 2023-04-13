;;; my-copilot.el --- Copilot customization
;;; Commentary:

;;; Code:
(require 'copilot)

(setq copilot-idle-delay 1)

(defun cg/copilot-complete-accept ()
  "Complete or accept copilot."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (forward-line))
    (copilot-complete)))

(add-hook 'copilot-mode-hook
          (lambda()
            (define-key copilot-mode-map (kbd "C-c M-RET") #'cg/copilot-complete-accept)
            (define-key copilot-mode-map (kbd "C-c M-x") #'copilot-clear-overlay)
            (define-key copilot-mode-map (kbd "C-c M-n") #'copilot-next-completion)
            (define-key copilot-mode-map (kbd "C-c M-p") #'copilot-previous-completion)
            (define-key copilot-mode-map (kbd "C-c M-f") #'copilot-accept-completion-by-word)
            (define-key copilot-mode-map (kbd "C-c M-e") #'copilot-accept-completion-by-line)
            ))


(provide 'my-copilot)
;;; my-copilot.el ends here
