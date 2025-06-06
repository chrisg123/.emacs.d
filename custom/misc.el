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
(require 'man)
(require 'grep)
(require 'my-flycheck)
(setq column-number-mode t)
(show-paren-mode t)

(global-flycheck-mode)
(when (boundp 'global-xah-math-input-mode)
  (global-xah-math-input-mode t))

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


;; Remove background color for transparency. Commented out because
;; ":background 'unspecified-bg'" in customize-set-face suffices.
;; (set-background-color nil)

;; Ensure packages know it's a dark background
(setq frame-background-mode 'dark)

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

(defvar visual-basic-mode-indent)
(defvar visual-basic-mode-map)

(defvar visual-basic-compilation-finished-functions '())
(defvar visual-basic-run-command)

(defun visual-basic-compilation-finished (buffer desc)
  "BUFFER, DESC."
  (interactive)
  (message "Buffer %s: %s" buffer desc)

  (when visual-basic-compilation-finished-functions
    (let ((xs visual-basic-compilation-finished-functions))
      (setq visual-basic-compilation-finished-functions '())
      (mapc (lambda(x) (eval x)) xs)
      ))
  )


(defun visual-basic-run()
  "Run `\\[visual-basic-run-command] asyncronously.  With a `\\[universal-argument]' \
prefix, `compile-command` is run before `visual-basic-run-command`."
  (interactive)
  (visual-basic-kill-async-buffer)
  (if current-prefix-arg
      (progn
        (add-to-list
         'visual-basic-compilation-finished-functions
         (function
          (let ((dir
                 (locate-dominating-file
                  "." (lambda (parent)
                        (directory-files parent nil ".*\\.vbproj")))))
            (async-shell-command (concat "cd '" dir "' && " visual-basic-run-command) ))))
        (visual-basic-compile))
    (let ((dir
           (locate-dominating-file
            "." (lambda (parent)
                  (directory-files parent nil ".*\\.vbproj")))))
      (async-shell-command (concat "cd '" dir "' && " visual-basic-run-command) ))

    ))

(defun visual-basic-kill-async-buffer ()
  "__________."
  (interactive)
  (setq kill-buffer-query-functions
        (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  (kill-matching-buffers "*Async Shell Command*" 0 t)
  )

(defun visual-basic-compile ()
  "Compile visual-basic project."
  (interactive)
  (let ((dir
         (locate-dominating-file
          "." (lambda (parent)
                (directory-files parent nil ".*\\.vbp")))))
    (compile (concat "cd '" dir "' && " "./build.sh"))))


(add-hook 'visual-basic-mode-hook
          (lambda()
            (setq visual-basic-mode-indent 4)
            (setq-default indent-tabs-mode nil)
            (define-key visual-basic-mode-map (kbd "C-c w")
              (lambda()
                (interactive) (print "No whitespace cleanup for vb6.")))
            (define-key visual-basic-mode-map (kbd "C-c C-p") 'visual-basic-beginning-of-defun)
            (define-key visual-basic-mode-map (kbd "C-c C-n") 'visual-basic-end-of-defun)
            (define-key visual-basic-mode-map (kbd "C-c C-c") 'visual-basic-compile)
            (define-key visual-basic-mode-map (kbd "C-c C-r") 'visual-basic-run)
            (define-key visual-basic-mode-map (kbd "C-c TAB") 'indent-region)
            (setq compile-command "./build.sh")
            (setq visual-basic-run-command "./build.sh -r")
            (setq compilation-read-command nil)
            (add-hook 'compilation-finish-functions 'visual-basic-compilation-finished)
            (setq grep-find-ignored-directories (append grep-find-ignored-directories (list "TAGS")))
            ))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

(defun add-include-path (path &optional buffer checker)
  "Add PATH to flycheck and semantic include-path.  Optional BUFFER."
  (interactive
   (list (string-trim-right (read-directory-name "Include path: "))))
  (let ((full-path (expand-file-name path)))
    (if (file-directory-p full-path)
        (if (y-or-n-p (format "Path: `%s'\nAdd path to includes? " full-path))
            (progn
              (message (format "buffer: %s" buffer))
              (let ((current-checker (or checker flycheck-checker)))
                (cond
                 ((eq current-checker 'c/c++-gcc)
                  (add-to-list 'flycheck-gcc-include-path full-path))
                 ((eq current-checker 'c/c++-emscripten)
                  (add-to-list 'flycheck-emscripten-include-path full-path))))
              (if buffer (switch-to-buffer buffer))
              (semantic-add-system-include full-path)
              (flycheck-buffer)
              )
          (message "Canceled."))
      (message "Path is not a directory."))))


(defun extra-include(path)
  "Load includes from `.extra_include' file in PATH."
  (interactive
   (list (string-trim-right (read-directory-name
                             "Path to `.extra_include' parent directory: "))))
  (let ((xfile (format "%s/.extra_include"
                       (replace-regexp-in-string "/*\s*$" "" path)))
        (buffer (current-buffer))
        (checker flycheck-checker))
    (if (file-exists-p xfile)
        (progn
          (message "file exists")
          (with-temp-buffer
            (insert-file-contents xfile)
            (mapcar (lambda (x) (add-include-path x buffer checker))
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

;; bash-completion seems to be messing up shell-command completion
;; (require 'bash-completion)
;; (bash-completion-setup)

(setq magit-diff-hide-trailing-cr-characters t)

(setq-default compilation-scroll-output t)

(defun center-buffer()
  (interactive)
  (set-window-margins nil (/ (- (frame-width) 125) 2) (/ (- (frame-width) 125) 2))
  )
(defun uncenter-buffer()
  (interactive)
  (set-window-margins nil nil nil)
  )

(add-hook 'vterm-mode-hook
          (lambda()
            (define-key vterm-mode-map (kbd "C-c C-v") 'vterm-copy-mode)
            (define-key vterm-mode-map (kbd "C-c C-x C-v") 'vterm-copy-mode-done)
            ))
(add-hook 'vterm-mode-hook
          (lambda()
            (define-key vterm-mode-map (kbd "C-c C-v") 'vterm-copy-mode)
            ))
(add-hook 'vterm-copy-mode-hook
          (lambda()
            (define-key vterm-copy-mode-map (kbd "C-c C-x C-v") 'vterm-copy-mode-done)
            ))

(defun beautify-json ()
  ;; https://coderwall.com/p/2vnxaw/beautify-json-in-emacs
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "mupdf")))

(defun copy-diff-region ()
  "Copy diff region without + or - markers."
  (interactive)
  (deactivate-mark)
  (let ((text (buffer-substring-no-properties
               (region-beginning) (region-end))))
    (kill-new (replace-regexp-in-string "^[\\+\\-]" "" text))))

(add-hook 'sh-mode-hook (lambda () (setq flycheck-checker 'sh-shellcheck)))

(defun sanitize-sql-region (start end)
  "Remove double quotes and the 'dbo.' qualifier from the selected region containing a SQL query."
  (interactive "r") ;; Enable region selection in interactive mode
  (let ((query (buffer-substring-no-properties start end)))
    (let ((sanitized-query (replace-regexp-in-string "\"" "" query)))
      (setq sanitized-query (replace-regexp-in-string "\\b\\(dbo\\.\\)" "" sanitized-query))
      (delete-region start end)
      (insert sanitized-query))))

(defun copy-visible-region (beg end)
  "Copy the visible region between BEG and END."
  (interactive "r")
  (let ((pos beg)
        (accum ""))
    (while (< pos end)
      (let ((next (or (next-single-property-change pos 'invisible nil end) end)))
        (unless (get-text-property pos 'invisible)
          (setq accum (concat accum (buffer-substring pos next))))
        (setq pos next)))
    (kill-new accum)
    (message "Copied visible text only.")))

(defun my-trace-log-setup ()
  "When visiting a file called `trace.log`, make it read-only and turn on auto-revert."
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name) "trace.log"))
    (read-only-mode 1)
    (auto-revert-mode 1)))

(add-hook 'find-file-hook #'my-trace-log-setup)
(defun my-push-files ()
  "Find the nearest .git directory and run push.sh asynchronously."
  (interactive)
  (let* ((dir (locate-dominating-file "." ".git"))
         (expanded-dir (and dir (expand-file-name dir))))
    (if expanded-dir
        (async-shell-command (concat "cd " (shell-quote-argument expanded-dir) " && ./push.sh"))
      (message "No .git directory found in parent directories."))))

(defun my-pull-files ()
  "Find the nearest .git directory and run pull.sh asynchronously."
  (interactive)
  (let* ((dir (locate-dominating-file "." ".git"))
         (expanded-dir (and dir (expand-file-name dir))))
    (if expanded-dir
        (async-shell-command (concat "cd " (shell-quote-argument expanded-dir) " && ./pull.sh"))
      (message "No .git directory found in parent directories."))))

(defun my-push-or-pull-files (arg)
  "Push or pull files.  If ARG then pull else push."
  (interactive "P")
  (if arg
      (my-pull-files)
    (my-push-files)))

(global-set-key (kbd "C-c C-u") 'my-push-or-pull-files)

(defun my-nxml-mode-keys()
  (local-set-key (kbd "C-c C-e e") 'sgml-skip-tag-forward)
  (local-set-key (kbd "C-c C-e b") 'sgml-skip-tag-backward)
  )

(add-hook 'nxml-mode-hook 'my-nxml-mode-keys)

(add-hook 'with-editor-mode-hook
          (lambda ()
            ;; no lock‑files
            (setq-local create-lockfiles nil)
            ;; no auto‑revert
            (when (bound-and-true-p auto-revert-mode)
              (auto-revert-mode -1))
            ;; never ask about disk‑change here
            (setq-local ask-user-about-supersession-threat nil)))

(defvar my/compile-root nil
  "If non-nil, use this directory as the root for all compile commands.
Set interactively with `my/set-compile-root`.")

(defvar my/last-compile-root nil
  "The directory of the last successful project compile (from auto-detect).")

(defun my/set-compile-root (dir)
  "Manually override `my/compile-root` to DIR."
  (interactive "DCompile root directory: ")
  (setq my/compile-root (expand-file-name dir))
  (message "Compile root explicitly set to: %s" my/compile-root))

(defun my/clear-compile-root ()
  "Clear the manual override in `my/compile-root`."
  (interactive)
  (setq my/compile-root nil)
  (message "Compile root override cleared."))

(defun my/detect-build-root (pattern)
  "Detect build root using PATTERN."
  (locate-dominating-file
   default-directory
   (lambda (parent)
     (and (directory-files parent nil pattern)
          (or (file-exists-p (expand-file-name "build.py" parent))
              (file-exists-p (expand-file-name "build.sh" parent)))))))

(defun my/compile-root (pattern)
  "Decide where to run a build command using PATTERN."
  (let* ((detected (my/detect-build-root pattern)))
    (or my/compile-root
        (when detected
          (setq my/last-compile-root (expand-file-name detected)))
        my/last-compile-root
        (user-error "No compile root found; set `my/compile-root' or open a project file"))))

(defvar my/bg-transparent t
  "Non-`nil` means the default background is transparent.")

(defun my/toggle-bg-transparent ()
  "Toggle `default` face background between transparent and black."
  (interactive)
  (if my/bg-transparent
      (set-face-attribute 'default nil :background "black")
    (set-face-attribute 'default nil :background "unspecified-bg"))
  (setq my/bg-transparent (not my/bg-transparent))
  (message "Background → %s"
           (if my/bg-transparent "transparent" "black")))

;; bind it to a key of your choice, e.g. F5
(global-set-key (kbd "<f5>") #'my/toggle-bg-transparent)


(provide 'misc)
;;; misc.el ends here
