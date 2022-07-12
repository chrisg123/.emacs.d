;;; misc.el --- Miscellaneous Emacs settings
;;; commentary:
;; A place for miscellaneous Emacs settings.

;;; Code:

;; display the current column number
(require 'xterm-color)
(require 'compile)
(require 'magit)
(require 'vbnet-mode)
(require 'flycheck)

(setq column-number-mode t)
(show-paren-mode t)

(global-flycheck-mode)
(when (boundp 'global-xah-math-input-mode)
      (global-xah-math-input-mode t)
      )

(semantic-mode 1)

(menu-bar-mode -1)

;; Disable the splash screen
(setq inhibit-splash-screen t)

(defun is-gnu-screen ()
  "Check if gnu screen is running."
  (cond ((getenv "STY") t)))

(if 1 ;; used to use is-gnu-screen but set to always disable italic
    ;; since it caused issues when ssh'd into a remote machine
    ;; while using gnu-screen localy
    ;; Disable italic. Does not play well with gnu screen.
    (progn (set-face-italic 'font-lock-comment-face nil)
	       (set-face-italic 'font-lock-doc-face nil)
	       (set-face-italic 'font-lock-preprocessor-face nil)))

;; remove background color so transparency works.
;; TODO: maybe check if compositing is enabled first.
(set-background-color nil)

;; ANSI Color https://stackoverflow.com/a/23382008/2974621
(require 'ansi-color)
(defun display-ansi-colors ()
  "Display ANSI color codes."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq scroll-conservatively most-positive-fixnum)

(delete-selection-mode t)

(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'" . sh-mode))

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(push '("\\.\\(?:frm\\|\\(?:ba\\|cl\\|vb\\)s\\)\\'" . visual-basic-mode)
      auto-mode-alist)

(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.vb\\'" . vbnet-mode)) auto-mode-alist))

(defvar visual-basic-mode-indent)
(defvar visual-basic-mode-map)


(defvar vbnet-compilation-finished-functions '())
(defvar vbnet-run-command)


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
        (add-to-list 'vbnet-compilation-finished-functions
                     (function (async-shell-command vbnet-run-command)))
        (compile compile-command))

    (async-shell-command vbnet-run-command))
  )

(defun vbnet-kill-async-buffer ()
  "__________."
  (interactive)
  (setq kill-buffer-query-functions
        (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  (kill-matching-buffers "*Async Shell Command*" 0 t)
  )

(add-hook 'visual-basic-mode-hook
          (lambda()
            (setq visual-basic-mode-indent 4)
            (setq-default indent-tabs-mode nil)
            (define-key visual-basic-mode-map (kbd "C-c w")
              (lambda()
                (interactive) (print "No whitespace cleanup for vb6.")))
            (define-key visual-basic-mode-map (kbd "C-c C-p") 'visual-basic-beginning-of-defun)
            (define-key visual-basic-mode-map (kbd "C-c C-n") 'visual-basic-end-of-defun)
            ))

(add-hook 'vbnet-mode-hook
          (lambda()
            (define-key vbnet-mode-map (kbd "C-c C-c") 'compile)
            (define-key vbnet-mode-map (kbd "C-c C-r") 'vbnet-run)
            (define-key vbnet-mode-map (kbd "C-c C-k") 'vbnet-kill-async-buffer)
            (setq compile-command "./build.sh")
            (setq vbnet-run-command "./build.sh -r")
            (setq compilation-read-command nil)
            (add-hook 'compilation-finish-functions 'vbnet-compilation-finished)
            ))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

(defun add-include-path (path &optional buffer)
  "Add PATH to flycheck and semantic include-path.  Optional BUFFER."
  (interactive
   (list (string-trim-right (read-directory-name "Include path: "))))
  (if (file-directory-p path)
      (if (y-or-n-p (format "Path: `%s'\nAdd path to includes? " path))
          (progn
            (message (format "buffer: %s" buffer))
            (add-to-list 'flycheck-gcc-include-path path)
            (if buffer (switch-to-buffer buffer))
            (semantic-add-system-include path)
            (flycheck-buffer)
            )
        (message "Canceled."))
    (message "Path is not a directory.")))

(defun extra-include(path)
  "Load includes from `.extra_include' file in PATH."
  (interactive
   (list (string-trim-right (read-directory-name
                             "Path to `.extra_include' parent directory: "))))
  (let ((xfile (format "%s/.extra_include"
                       (replace-regexp-in-string "/*\s*$" "" path)))
        (buffer (current-buffer)))
    (if (file-exists-p xfile)
        (progn
          (message "file exists")
          (with-temp-buffer
            (insert-file-contents xfile)
            (mapcar (lambda (x) (add-include-path x buffer))
                    (split-string (buffer-string) "\n" t))
            )
          )
      (message (format "File not found: `%s'" xfile)))))

(defun er-kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
(global-set-key (kbd "C-c k") #'er-kill-other-buffers)

(defun set-title-gnu-screen ()
  "Set title for GNU screen."
  (if (getenv "STY")
      (let ((title (concat "emacs:" (buffer-name)))
            (esc "\ek%s\e\\"))
        (send-string-to-terminal (format esc title)))))

(defvar window-selection-change-functions '())
(add-to-list 'window-selection-change-functions
             (lambda(_) (set-title-gnu-screen)))

(defun copy-buffer-file-name()
  "Copy 'buffer-file-name' to xsel clipboard."
  (interactive)
  (shell-command (format "printf %s | xsel --clipboard -i" (buffer-file-name))))

(defun insert-uuid()
  "Insert uuid at point."
  (interactive)
  (insert (replace-regexp-in-string "\n$" ""
                                    (shell-command-to-string "uuidgen"))))
(defun my-reverse-region (beg end)
  "Reverse characters between BEG and END."
  (interactive "r")
  (let ((region (buffer-substring beg end)))
    (delete-region beg end)
    (insert (nreverse region))))

(defun xterm-color-colorize-shell-command-output ()
  "Colorize `shell-command' output."
  (let ((bufs
         (seq-remove
          (lambda (x)
            (not (or (string-prefix-p " *Echo Area" (buffer-name x))
                     (string-prefix-p "*Shell Command" (buffer-name x)))))
          (buffer-list))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (xterm-color-colorize-buffer)))))

(defun xterm-color-colorize-shell-command-output-advice (proc &rest rest)
  "PROC REST."
  (xterm-color-colorize-shell-command-output))

(advice-add 'shell-command :after #'xterm-color-colorize-shell-command-output-advice)

;; https://stackoverflow.com/a/63710493/2974621
(setq compilation-environment '("TERM=xterm-256color"))
(defun my/advice-compilation-filter (f proc string)
  "F PROC STRING."
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

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
     ;;(vbnet-of-type-declaration-font-lock-find)
     )
   'append))

;; bash-completion seems to be messing up shell-command completion
;; (require 'bash-completion)
;; (bash-completion-setup)

(setq magit-diff-hide-trailing-cr-characters t)

(setq compilation-scroll-output t)

(provide 'misc)
;;; misc.el ends here
