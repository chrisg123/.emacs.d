;;; my-haskell.el --- Haskell customization
;;; Commentary:

;;; Code:

(defun pretty-lambdas-haskell ()
  "Show lambda symbol instead of backslash."
  (font-lock-add-keywords
   nil `((,(concat "\\(" (regexp-quote "\\") "\\)")
	  (0 (progn (compose-region (match-beginning 1) (match-end 1)
				    ,(make-char 'greek-iso8859-7 107))
		    nil))))))

(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)
(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(provide 'my-haskell)

;;; my-haskell.el ends here
