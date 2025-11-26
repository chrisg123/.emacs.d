;;; my-vbnet.el --- Vbnet customization
;;; Commentary:

;;; Code:

(require 'misc)
(require 'vbnet-mode)
(require 'compile)
(require 'hideshow)

(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.vb\\'" . vbnet-mode)) auto-mode-alist))

(defvar vbnet-compilation-finished-functions '())
(defvar vbnet-run-command)
(defvar vbnet-run-command-remote)
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
  "Run =vbnet-run-command= asynchronously.
With a =\\[universal-argument]' prefix, =vbnet-run-command-remote= is run instead."
  (interactive)
  (vbnet-kill-async-buffer)
  (let* ((root (my/resolve-compile-root "\\.vbproj\\'" t))
         (default-directory root)
         (vbnet-run-command (concat (shell-quote-argument (vbnet-choose-compiler-script root)) " -r")))
    (if current-prefix-arg
        (async-shell-command vbnet-run-command-remote)
      (async-shell-command vbnet-run-command))))

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

(defun vbnet-choose-compiler-script (root)
  "Return the build script in ROOT, preferring build.py but defaulting to build.sh."
  (let ((py (expand-file-name "build.py" root))
        (sh (expand-file-name "build.sh" root)))
    (if (file-exists-p py)
        py
      ;; Even if build.sh isn't present, we still return its expected path
      ;; so that your hook never signals an error during initialization.
      sh)))


(defun vbnet-compile (&optional force)
  "Compile VB.NET project.  FORCE."
  (interactive "P")
  (let* ((root (my/resolve-compile-root "\\.vbproj\\'" force))
         (default-directory root)
         (compile-command (shell-quote-argument (vbnet-choose-compiler-script root))))
    ;; Set custom display rules for the *compilation* buffer just before compiling
    (let ((display-buffer-alist
           (cons '("\\*compilation\\*"
                   display-compilation-buffer
                   (reusable-frames . t))
                 display-buffer-alist)))
      (compile compile-command))))

;; (defun vbnet-hs-forward-sexp (arg)
;;   "Move point from a `#Region` line to the matching `#End Region`.
;; ARG is ignored; we always move forward one region.  Nested regions are
;; handled by keeping a depth counter."
;;   (ignore arg)
;;   (let ((depth 1))
;;     ;; Start searching from the next line so we don't immediately re-hit the
;;     ;; same #Region we’re sitting on.
;;     (forward-line 1)
;;     (while (and (> depth 0)
;;                 (re-search-forward
;;                  "^[ \t]*#\\(End[ \t]+Region\\b\\|Region\\b\\)" nil t))
;;       (if (string-prefix-p "End" (match-string 1))
;;           (setq depth (1- depth))   ; found #End Region
;;         (setq depth (1+ depth)))))) ; found nested #Region

;; (defun my-vbnet-hs-setup()
;;   "Setup hideshow mode."
;;   (add-to-list 'hs-special-modes-alist
;;                '(vbnet-mode
;;                  "^[ \t]*#Region\\b"          ;; block start
;;                  "^[ \t]*#End[ \t]+Region\\b" ;; block end
;;                  "'"                          ;; comment start
;;                  vbnet-hs-forward-sexp        ;; forward-sexp-func
;;                  nil))                        ;; adjust-beg-func
;;   (hs-minor-mode 1))

(defun vbnet-hs--forward-region ()
  "Move point from a `#Region` line to its matching `#End Region`."
  (let ((depth 1))
    (forward-line 1)
    (while (and (> depth 0)
                (re-search-forward
                 "^[ \t]*#\\(End[ \t]+Region\\b\\|Region\\b\\)" nil t))
      (if (string-prefix-p "End" (match-string 1))
          (setq depth (1- depth))  ; found #End Region
        (setq depth (1+ depth)))))) ; found nested #Region


(defun vbnet-hs--forward-block ()
  "Move point from a VB.NET block start to the matching End Foo.
Uses `block-start` and `block-end` regexes from `vbnet-regexp-alist`."
  (let* ((start-re (vbnet-regexp 'block-start))
         (end-re   (vbnet-regexp 'block-end))
         (depth 1))
    (forward-line 1)
    (while (and (> depth 0)
                (re-search-forward (concat start-re "\\|" end-re) nil t))
      (let ((match (match-string 0)))
        (if (string-match-p end-re match)
            (setq depth (1- depth)) ; found End Sub/End Class/etc
          (setq depth (1+ depth))))))) ; found nested block (Class in Namespace, etc)

(defun vbnet-hs-forward-sexp (arg)
  "Move point forward one VB.NET foldable unit for `hs-minor-mode`.  Ignore ARG.

If point is on a `#Region` line, fold to its `#End Region`.
Otherwise, if it matches `block-start`, fold to the corresponding `End Foo`."
  (ignore arg)
  (beginning-of-line)
  (cond
   ;; #Region / #End Region pair
   ((looking-at "^[ \t]*#Region\\b")
    (vbnet-hs--forward-region))

   ;; Sub/Function/Class/Module/Property/Interface/Enum/Type
   ((looking-at (vbnet-regexp 'block-start))
    (vbnet-hs--forward-block))

   ;; Fallback – try treating this line as a block start anyway
   (t
    (vbnet-hs--forward-block))))

(defun vbnet-hs-hide-all-regions ()
  "Hide all #Region blocks in the current buffer using hideshow."
  (interactive)
  (when (bound-and-true-p hs-minor-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#Region\\b" nil t)
        (goto-char (match-beginning 0))
        ;; At a #Region line, hideshow will use `vbnet-hs-forward-sexp`
        ;; to find the matching #End Region and hide the block.
        (hs-hide-block)
        ;; Move to the next line so we don't re-hide the same region.
        (forward-line 1)))))

(defun my-vbnet-hs-setup ()
  "Setup hideshow for `vbnet-mode` using regions and VB blocks."
  (interactive)
  (add-to-list
   'hs-special-modes-alist
   `(vbnet-mode
     ,(concat "^[ \t]*#Region\\b\\|" (vbnet-regexp 'block-start)) ; start
     ,(concat "^[ \t]*#End[ \t]+Region\\b\\|" (vbnet-regexp 'block-end)) ; end
     "'"                                ; comment start
     vbnet-hs-forward-sexp              ; forward-sexp-func
     nil))                              ; adjust-beg-func
  (hs-minor-mode 1)
  (vbnet-hs-hide-all-regions))

(add-hook 'vbnet-mode-hook
          (lambda()
            (my-vbnet-hs-setup)
            (define-key vbnet-mode-map (kbd "C-c C-p") 'vbnet-moveto-beginning-of-block)
            (define-key vbnet-mode-map (kbd "C-c C-n") 'vbnet-moveto-end-of-block)
            (define-key vbnet-mode-map (kbd "C-c C-c") 'vbnet-compile)
            (define-key vbnet-mode-map (kbd "C-c C-r") 'vbnet-run)
            (define-key vbnet-mode-map (kbd "C-c C-t") 'vbnet-run-tests)
            (define-key vbnet-mode-map (kbd "C-c C-k") 'vbnet-kill-async-buffer)
            (define-key vbnet-mode-map (kbd "C-c TAB") 'indent-region)
            (define-key vbnet-mode-map (kbd "C-c C-f") 'hs-toggle-hiding)
            (setq vbnet-run-command-remote "./run_remote.sh")
            (setq compilation-read-command nil)
            (setq vbnet-run-tests-command "./testrun.py")
            (add-hook 'compilation-finish-functions 'vbnet-compilation-finished)
            ))

(defconst brace-regexp
  "[^{]{[^{}]*}")

(defconst vbnet-of-type-declaration-regexp
  "\\(?:Of[[:space:]]*\\([^)]*\\)\\)"
  )

(defconst vbnet-interpolated-string-regexp
  ;;  \($"     start with $"
  ;;  \(\"\"  or a doubled quote
  ;;  \|     ...
  ;;  [^"]   any char that is not a double‐quote (this includes newline!)
  ;;  \)*    …repeated any number of times
  ;;  \"     closing "
  "\\$\"\\(\"\"\\|[^\"]\\)*\"")

(defun vbnet-string-interpolation-font-lock-find (limit)
  "Fontify VB interpolated strings ($\"…\") including multiline, up to LIMIT."
  (while (re-search-forward vbnet-interpolated-string-regexp limit t)
    ;; 1) Highlight the entire interpolated string as a string:
    (put-text-property (match-beginning 0)
                       (match-end 0)
                       'face 'font-lock-string-face)

    ;; 2) Now scan INSIDE that region for {…} and highlight the inner expression:
    (let ((str-start (match-beginning 0))
          (str-end   (match-end 0)))
      (save-excursion
        (goto-char str-start)
        (while (re-search-forward "{\\([^}]*\\)}" str-end t)
          ;; a) Highlight the opening "{" itself with function-name face:
          (put-text-property (match-beginning 0)
                             (1+ (match-beginning 0))
                             'face 'font-lock-function-name-face)
          ;; b) Highlight the closing "}" itself with function-name face:
          (put-text-property (1- (match-end 0))
                             (match-end 0)
                             'face 'font-lock-function-name-face)
          ;; c) Highlight what's between { and } with variable-name face:
          (put-text-property (1+ (match-beginning 0))
                             (1- (match-end 0))
                             'face 'font-lock-variable-name-face)))))
  nil)

;; ----------------------------------------------------------------------------
;; 2) Keep the region‐extender hook in vbnet‐mode-hook (buffer-local).
;; ----------------------------------------------------------------------------

(defun my-vbnet-extend-region-to-interpolated-string ()
  "If we’re inside a VB `$\"…\"` block, extend font-lock-beg/end to cover it all."
  (when (and (boundp 'font-lock-beg)
             (boundp 'font-lock-end))
    (let ((ppss-beg (save-excursion
                      (goto-char font-lock-beg)
                      (syntax-ppss)))
          (ppss-end (save-excursion
                      (goto-char font-lock-end)
                      (syntax-ppss))))
      (when (or (nth 3 ppss-beg) (nth 3 ppss-end))
        (let ((string-start (min
                             (or (nth 8 ppss-beg) most-positive-fixnum)
                             (or (nth 8 ppss-end) most-positive-fixnum)))
              string-end)
          (save-excursion
            (goto-char font-lock-end)
            (condition-case nil (forward-sexp) (error nil))
            (setq string-end (point)))
          (setq font-lock-beg (min font-lock-beg string-start)
                font-lock-end (max font-lock-end string-end)))))))

(defun my-vbnet-setup-fontlock-extensions ()
  "Register the interpolation-region extender (buffer-local) in vbnet-mode."
  ;; Add our region-extender. ‘t’ makes it buffer-local, so it only affects vbnet buffers.
  (add-hook 'font-lock-extend-region-functions
            #'my-vbnet-extend-region-to-interpolated-string
            nil  ;; APPEND doesn’t matter here
            t)) ;; Make it buffer-local

(add-hook 'vbnet-mode-hook #'my-vbnet-setup-fontlock-extensions)



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
   'prepend))

(defun find-vbnet-subroutines-and-functions ()
  "Use `occur` to find all VB.NET Subroutines and Functions in the current buffer."
  (interactive)
  (occur "^\s*\\(Public\\|Private\\|Protected\\|Friend\\|Protected Friend\\|Public Shared\\|Private Shared\\|Protected Shared\\|Friend Shared\\|Protected Friend Shared\\)?\s*\\(Sub\\|Function\\)\s+"))


(defun vbnet-remove-trace-lines ()
  "Kill every line that contains a Tracer.Trace call."
  (interactive)
  (flush-lines "^[[:blank:]]*Tracer\\.Trace.*$"))

(defun vbnet-trace-expression ()
  "If current line contains `Dim <var> As <type> =`, insert a trace line under it."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at
           "^\\s-*Dim\\s-+\\([A-Za-z0-9_]+\\)\\s-+As\\s-+[A-Za-z0-9_]+\\s-*=")
      (let ((var (match-string 1)))
        ;; move to end of line and insert trace
        (end-of-line)
        (newline-and-indent)
        (insert (format "Tracer.Trace($\"%s: {%s}\")" var var))))))
(provide 'my-vbnet)

;;; my-vbnet.el ends here
