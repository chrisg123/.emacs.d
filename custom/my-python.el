;;; my-python.el --- Python customization
;;; Commentary:

;;; Code:
(require 'python)
(require 'pyvenv)
(require 'elpy)
(require 'elpy-rpc)
(require 'format-all)
(require 'rx)
(require 'grep)

(defvar elpy-rpc-backend)
(setq elpy-rpc-backend "jedi")
(setq eldoc-idle-delay 1)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4)
            (add-to-list 'format-all-formatters '("Python" yapf))
            (python-extra-font-lock)
            (setq grep-find-ignored-directories (append grep-find-ignored-directories (list "venv")))
            (tree-sitter-mode)))

(defun my-venv()
  "Setup venv."
  (interactive)
  (let ((venv (read-directory-name "venv directory: ")))
    (pyvenv-activate venv)
    (setq elpy-rpc-virtualenv-path venv)
    (elpy-enable)
    ))


(defun python-extra-font-lock()
  "Extra font locking."
  (interactive)
  (font-lock-add-keywords ;; example x: Type[int]
   nil `((,(rx-to-string
            `(and ": " (group (+ (or alnum "."))) (* space) (or "[" ")" "," line-end))
            t) 1 'font-lock-type-face)))

  (font-lock-add-keywords ;; example:  [Type, ...]
   nil `((,(rx-to-string
            `(and "[" (* space) (group (+ alnum)) (? space) ","  (* any) "]") t)
          (1 'font-lock-type-face append) )) t)

  (font-lock-add-keywords ;; example:  [..., Type, ...]
   nil `((,(rx-to-string
            `(and "[" (* any) "," (* space) (group (+ alnum)) (* space) "," (* any) "]") t)
          (1 'font-lock-type-face append) )) t)

  (font-lock-add-keywords ;; example:  [..., Type]
   nil `((,(rx-to-string
            `(and "[" (* any) "," (* space) (group (+ alnum)) (* space) "]") t)
          (1 'font-lock-type-face append) )) t)

  (font-lock-add-keywords ;; example:  [Type]
   nil `((,(rx-to-string `(and "[" (* space) (group (+ alnum)) (* space) "]") t)
          (1 'font-lock-type-face append) )) t)

  (font-lock-add-keywords ;; example:  [Type[OtherType[int]]
   nil `((,(rx-to-string
            `(and "[" (* space) (group (+ alnum)) (* space) "[" (* any) "]") t)
          (1 'font-lock-type-face append) )) t)

  (font-lock-add-keywords ;; example: -> QWidget:
   nil `((,(rx-to-string
            `(and "->" (* space) (group (+ alnum)) (* space) ":") t)
          1 'font-lock-type-face)))

  (font-lock-add-keywords ;; example: = QColor(
   nil `((,(rx-to-string
            `(and (or bow "(") (group upper (+ alnum)) (? ".") (* alnum) "(") t)
          1 'font-lock-type-face)))
  )

(defun python-run ()
  "Run `run.sh` from the current directory or the nearest parent directory."
  (interactive)
  (let ((dir (locate-dominating-file default-directory
                                     (lambda (parent)
                                       (file-exists-p (expand-file-name "run.sh" parent))
                                       ))))
    (if dir
        (let ((expanded-dir (expand-file-name dir)))
          (async-shell-command (concat "cd '" expanded-dir "' && ./run.sh"))
          (message "Executing run.sh in %s" expanded-dir))
      (message "No run.sh found in this directory or its parents."))))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c C-r") 'python-run)))

(provide 'my-python)

;;; my-python.el ends here
