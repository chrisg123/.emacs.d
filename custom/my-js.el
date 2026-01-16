;;; my-js.el --- Js customization
;;; Commentary:

;;; Code:
(require 'js)

(defun my-js-mode-keys ()
  "Set custom keybindings for JavaScript mode."
  (local-set-key (kbd "C-c C-e e") 'end-of-defun)
  (local-set-key (kbd "C-c C-e b") 'beginning-of-defun))

(add-hook 'js-mode-hook 'my-js-mode-keys)
;;(add-hook 'js-ts-mode-hook 'my-js-mode-keys) ;; If using Tree-sitter


(defun js-run ()
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

(add-hook 'js-mode-hook
          (lambda ()
            (setq-local tab-width 2)
            (setq-local js-indent-level 2)
            (setq-local indent-tabs-mode nil)
            (define-key js-mode-map (kbd "C-c C-r") 'js-run)))

(provide 'my-js)

;;; my-js.el ends here
