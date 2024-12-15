;;; my-cxx.el --- C++ customization
;;; Commentary:

;;; Code:
(defvar c++-mode-map)
(defvar cxx-run-command)
(defvar cxx-compilation-finished-functions '())

(add-hook
 'c++-mode-hook
 (lambda()
   (semantic-mode 1)
   (setq-default flycheck-checker 'c/c++-gcc)
   (setq c-hungry-delete-key nil)
   (setq compilation-read-command nil)
   (setq cxx-run-command "./run.sh")
   (define-key c++-mode-map (kbd "M-.") 'semantic-ia-fast-jump)
   (define-key c++-mode-map (kbd "C-c C-c") 'cxx-compile)
   (define-key c++-mode-map (kbd "C-c C-r") 'cxx-run)
   (add-hook 'compilation-finish-functions 'cxx-compilation-finished)
   ))

(defun cxx-compile ()
  "Compile cxx project."
  (interactive)
  (let ((dir
         (expand-file-name
          (locate-dominating-file
           "." (lambda (parent)
                 (directory-files parent nil "Makefile"))))))
    (compile (concat "cd '" dir "' && " "make -k"))))

(defun cxx-compilation-finished (buffer desc)
  "BUFFER, DESC."
  (interactive)
  (message "Buffer %s: %s" buffer desc)

  (when cxx-compilation-finished-functions
    (let ((xs cxx-compilation-finished-functions))
      (setq cxx-compilation-finished-functions '())
      (mapc (lambda(x) (eval x)) xs)
      ))
  )

(defun cxx-run()
  (interactive)
  (cxx-kill-async-buffer)
  (if current-prefix-arg
      (progn
        (add-to-list
         'cxx-compilation-finished-functions
         (function
          (let ((dir
                 (locate-dominating-file
                  "." (lambda (parent)
                        (or (directory-files parent nil "build")
                            (directory-files parent nil "Makefile"))))))
            (async-shell-command (concat "cd " dir " && " cxx-run-command) ))))
        (compile "make -k"))
    (let ((dir
           (locate-dominating-file
            "." (lambda (parent)
                  (or (directory-files parent nil "build")
                      (directory-files parent nil "Makefile"))))))
      (async-shell-command (concat "cd " dir " && " cxx-run-command) ))
    )
  )

(defun cxx-kill-async-buffer ()
  "__________."
  (interactive)
  (setq kill-buffer-query-functions
        (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  (kill-matching-buffers "*Async Shell Command*" 0 t)
  )

(provide 'my-cxx)
;;; my-cxx.el ends here
