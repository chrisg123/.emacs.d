;;; my-gptel.el --- gptel settings
;;; Commentary:
;;; Code:
(require 'gptel-openai-extras)
(require 'gptel-context)
(require 'gptel-transient)
(require 'gptel-org)
(require 'gptel-rewrite)
(require 'gptel)
(require 'my-gptel-llm-tools)

(defun my-gptel-set-keybindings ()
  "Set keybindings for gptel."
  (global-set-key (kbd "C-c g") (lambda (prefix) (interactive "p") (gptel-send prefix)))
  (global-set-key (kbd "C-c a") 'gptel-add))

(defun my-gptel-set-settings ()
  "Set a gptel SETTING to VALUE."
  (setq gptel-confirm-tool-calls t)
  (setq gptel-model 'gpt-5.2)
  (setq gptel-expert-commands t)
  (setq gptel-default-mode 'org-mode))

(defun my-gptel-setup-directives ()
  "Add custom directives to gptel directives."
  (let ((new-directives
         '((custom-directive . "You are a large language model living in Emacs and a helpful assistant. Respond concisely. All code should be wrapped in org-mode src blocks. vb.net code should use vbnet as the src type."))))
    (setq gptel-directives (append new-directives gptel-directives))))

(defun my-gptel-setup ()
  "Setup gptel."
  (my-gptel-set-settings)
  (my-gptel-set-keybindings)
  (my-gptel-setup-directives))

(my-gptel-setup)

(provide 'my-gptel)
;;; my-gptel.el ends here
