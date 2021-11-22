;;; my-semantic.el --- Semantic customization
;;; Commentary:

;;; Code:
(require 'semantic)

(setq semantic-default-submodes
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-highlight-func-mode))

(provide 'my-semantic)
;;; my-semantic.el ends here
