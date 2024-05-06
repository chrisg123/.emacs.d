;;; my-vbnet.el --- Vbnet customization
;;; Commentary:

;;; Code:

(require 'vbnet-mode)
(require 'compile)

(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.vb\\'" . vbnet-mode)) auto-mode-alist))

(defvar vbnet-compilation-finished-functions '())
(defvar vbnet-run-command)
(defvar vbnet-run-tests-command)

(defun vbnet-compilation-finished (buffer desc)
  "BUFFER, DESC."
  (interactive)
  (message "Buffer %s: %s" buffer desc)

  (when vbnet-compilation-finished-functions
    (let ((xs vbnet-compilation-finished-functions))
      (setq vbnet-compilation-finished-functions '())
      (mapc (lambda(x) (eval x)) xs)
      ))
  )

(defun vbnet-run()
  "Run `\\[vbnet-run-command] asyncronously.  With a `\\[universal-argument]' \
prefix, `compile-command` is run before `vbnet-run-command`."
  (interactive)
  (vbnet-kill-async-buffer)
  (if current-prefix-arg
      (progn
        (add-to-list
         'vbnet-compilation-finished-functions
         (function
          (let ((dir
                 (locate-dominating-file
                  "." (lambda (parent)
                        (directory-files parent nil ".*\\.vbproj")))))
            (async-shell-command (concat "cd '" dir "' && " vbnet-run-command) ))))
        (vbnet-compile))
    (let ((dir
           (locate-dominating-file
            "." (lambda (parent)
                  (directory-files parent nil ".*\\.vbproj")))))
      (async-shell-command (concat "cd '" dir "' && " vbnet-run-command) ))

    ))

(defun vbnet-run-tests()
  "."
  (interactive)
  (vbnet-kill-async-buffer)
  (if current-prefix-arg
      (progn
        (add-to-list
         'vbnet-compilation-finished-functions
         (function
          (let ((dir
                 (locate-dominating-file
                  "." (lambda (parent)
                        (directory-files parent nil ".*\\.vbproj")))))
            (my-async-shell-command vbnet-run-tests-command ))))
        (vbnet-compile))
    (let ((dir
           (locate-dominating-file
            "." (lambda (parent)
                  (directory-files parent nil ".*\\.vbproj")))))
      (my-async-shell-command (list "python" vbnet-run-tests-command) ))

    ))

(defun my-async-shell-command(cmd)
  "CMD."
  (let ((buf "*Testrun*"))
    (when (gnus-buffer-exists-p buf) (kill-buffer buf))
    (get-buffer-create buf)
    (switch-to-buffer-other-window buf)

    (insert "\t")
    (make-process
     :name "*Testrun*"
     :buffer "*Testrun*"
     :command cmd
     :connection-type 'pipe
     :sentinel (lambda(proc msg)
                 (when (equal "finished\n" msg)
                   (with-current-buffer "*Testrun*"
                     (compilation-mode)
                     (setq buffer-read-only nil)
                     (xterm-color-colorize-buffer)
                     (setq buffer-read-only t)
                     (goto-char (point-max))
                     (recenter -1)
                     ))))))

(defun vbnet-kill-async-buffer ()
  "__________."
  (interactive)
  (setq kill-buffer-query-functions
        (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  (kill-matching-buffers "*Async Shell Command*" 0 t)
  )


(defun display-compilation-buffer (buffer alist)
  "Display the compilation buffer in an appropriate manner based on current window configuration."
  (let ((single-window (one-window-p t))
        displayed)
    (setq displayed
          (or (display-buffer-reuse-window buffer alist)  ;; First try to reuse an existing window
              (if single-window
                  (let ((new-window (split-window-horizontally))) ;; If there's only one window, split it
                    ;; Display buffer in the new window and select it
                    (set-window-buffer new-window buffer)
                    (select-window new-window)
                    new-window)
                (display-buffer-use-some-window buffer alist))  ;; Try to use some existing window
              (display-buffer-below-selected buffer alist)  ;; Try to split below if not yet handled
              (display-buffer-pop-up-window buffer alist))) ;; As a fallback, pop up a new window
    displayed))

(defun vbnet-compile ()
  "Compile VB.NET project."
  (interactive)
  (let* ((dir (locate-dominating-file
               "." (lambda (parent)
                     (directory-files parent nil ".*\\.vbproj"))))
         (compile-command (concat "cd '" dir "' && " "./build.sh")))
    ;; Set custom display rules for the *compilation* buffer just before compiling
    (let ((display-buffer-alist
           (cons '("\\*compilation\\*"
                   display-compilation-buffer
                   (reusable-frames . t))
                 display-buffer-alist)))
      (compile compile-command))))

(add-hook 'vbnet-mode-hook
          (lambda()
            (define-key vbnet-mode-map (kbd "C-c C-p") 'vbnet-moveto-beginning-of-block)
            (define-key vbnet-mode-map (kbd "C-c C-n") 'vbnet-moveto-end-of-block)
            (define-key vbnet-mode-map (kbd "C-c C-c") 'vbnet-compile)
            (define-key vbnet-mode-map (kbd "C-c C-r") 'vbnet-run)
            (define-key vbnet-mode-map (kbd "C-c C-t") 'vbnet-run-tests)
            (define-key vbnet-mode-map (kbd "C-c C-k") 'vbnet-kill-async-buffer)
            (define-key vbnet-mode-map (kbd "C-c TAB") 'indent-region)
            (setq compile-command "./build.sh")
            (setq vbnet-run-command "./build.sh -r")
            (setq compilation-read-command nil)
            (setq vbnet-run-tests-command "./testrun.py")
            (add-hook 'compilation-finish-functions 'vbnet-compilation-finished)
            ))

(defconst brace-regexp
  "[^{]{[^{}]*}")

(defconst vbnet-string-interpolation-regexp
  "$\\('.*?[^\\]'\\|\".*?[^\\]\"\\)")

(defconst vbnet-of-type-declaration-regexp
  "\\(?:Of[[:space:]]*\\([^)]*\\)\\)"
  )

(defun vbnet-string-interpolation-font-lock-find (limit)
  "LIMIT."
  (while (re-search-forward vbnet-string-interpolation-regexp limit t)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face 'font-lock-string-face)
    (let ((start (match-beginning 0)))
      (while (re-search-backward brace-regexp start t)
        (put-text-property (1+ (match-beginning 0)) (match-end 0)
                           'face 'font-lock-type-face))))
  nil)

(defun re-seq (regexp string)
  "Match REGEXP in STRING and return a list."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun vbnet-enum-usage-font-lock-find (limit)
  "LIMIT."
  (let*
      ((enum-start
        (rx-to-string
         `(and line-start (* space) "Public Enum" (* space)
               (group (* alnum) line-end))))
       (keywords
        (re-seq enum-start
                (buffer-substring-no-properties (point-min) (point-max)))))
    (while (re-search-forward (mapconcat 'identity keywords "\\|") limit t)
      (put-text-property
       (match-beginning 0) (match-end 0)
       'face 'font-lock-type-face))
    nil
    ))

(defun vbnet-of-type-declaration-font-lock-find (limit)
  "LIMIT."
  (while (re-search-forward vbnet-of-type-declaration-regexp limit t)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face 'font-lock-type-face)
    ;; (let ((start (match-beginning 0)))
    ;;   (while (re-search-backward brace-regexp start t)
    ;;     (put-text-property (1+ (match-beginning 0)) (match-end 0)
    ;;                        'face 'font-lock-type-face)))
    )
  nil)

(with-eval-after-load 'vbnet-mode
  (font-lock-add-keywords
   'vbnet-mode
   `((vbnet-string-interpolation-font-lock-find)
     ;;(vbnet-enum-usage-font-lock-find)
     ;;(vbnet-of-type-declaration-font-lock-find)
     )
   'append))

(provide 'my-vbnet)

;;; my-vbnet.el ends here
