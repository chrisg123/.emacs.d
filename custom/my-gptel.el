;;; my-gptel.el --- gptel settings
;;; Commentary:
;;; Code:

(require 'gptel-curl)
(require 'gptel-context)
(require 'gptel-transient)
(require 'gptel-org)
(require 'gptel-rewrite)
(require 'gptel)

(defun my-gptel-set-keybindings ()
  "Set keybindings for gptel."
  (global-set-key (kbd "C-c g") (lambda (prefix) (interactive "p") (gptel-send prefix)))
  (global-set-key (kbd "C-c a") 'gptel-add))

(defun my-gptel-set-settings ()
  "Set a gptel SETTING to VALUE."
  (setq gptel-expert-commands t))

(defun my-gptel-setup ()
  "Setup gptel."
  (my-gptel-set-settings)
  (my-gptel-set-keybindings))

(my-gptel-setup)

(provide 'my-gptel)
;;; my-gptel.el ends here
