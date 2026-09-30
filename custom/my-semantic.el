;;; my-semantic.el --- Semantic customization
;;; Commentary:

;;; Code:
(require 'semantic)


(defun my-semantic-inhibit-svelte ()
  (derived-mode-p 'svelte-mode))

(add-to-list 'semantic-inhibit-functions
             #'my-semantic-inhibit-svelte)

(setq semantic-default-submodes
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-highlight-func-mode))

(provide 'my-semantic)
;;; my-semantic.el ends here
