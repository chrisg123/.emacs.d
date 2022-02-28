;;; my-python.el --- Python customization
;;; Commentary:

;;; Code:
(require 'python)
(require 'pyvenv)
(require 'elpy)
(require 'elpy-rpc)

(elpy-enable)
(setq elpy-rpc-backend "jedi")
(setq eldoc-idle-delay 1)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'python-mode-hook
          (lambda () (setq python-indent-offset 4)))


(defun my-venv()
  "Setup venv."
  (interactive)
  (let ((venv (read-directory-name "venv directory: ")))
    (pyvenv-activate venv)
    (setq elpy-rpc-virtualenv-path venv)
    ))

(provide 'my-python)

;;; my-python.el ends here
