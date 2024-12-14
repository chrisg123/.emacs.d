;;; my-swift.el --- Swift customization
;;; Commentary:

;;; Code:

(require 'dash)
(require 'lsp-mode)
(require 'org-src)
(require 'tramp-sh)
(require 'swift-mode)

(defvar lsp-sourcekit-executable)
(defvar my-xctoolchain
  "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin")

(defvar swift-compilation-finished-functions '())
(defvar swift-run-command)
(defvar swift-run-tests-command)

(defun swift-compilation-finished (buffer desc)
  "BUFFER, DESC."
  (interactive)
  (message "Buffer %s: %s" buffer desc)

  (when swift-compilation-finished-functions
    (let ((xs swift-compilation-finished-functions))
      (setq swift-compilation-finished-functions '())
      (mapc (lambda(x) (eval x)) xs)
      ))
  )


(eval-after-load 'lsp-mode
  (progn
    (require 'lsp-sourcekit)
    (setq lsp-sourcekit-executable
          "/opt/swift-build/usr/bin/sourcekit-lsp")))

(defun maybe-eval-string (string)
  "Maybe evaluate elisp in a given STRING."
  (or (ignore-errors
        (eval (car (read-from-string (format "(progn %s)" string))))
        ) string))


(defun org-babel-edit-prep:swift (babel-info)
  "Setup for lsp-mode in Org Src buffer using BABEL-INFO."
  (let ((beg org-src--beg-marker) (a nil) (b nil))
    (with-current-buffer (marker-buffer beg)
      (goto-char beg)
      (setq a (maybe-eval-string (->> babel-info caddr (alist-get :dir))))
      (setq b (maybe-eval-string (->> babel-info caddr (alist-get :tangle)))))
    (if a (setq-local default-directory a))
    (if b (setq-local buffer-file-name b))
    )
  (lsp))

(defun swift-kill-async-buffer ()
  "__________."
  (interactive)
  (setq kill-buffer-query-functions
        (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  (kill-matching-buffers "*Async Shell Command*" 0 t)
  )

(defun swift-run()
  "Run `\\[swift-run-command] asyncronously.  With a `\\[universal-argument]' \
prefix, `compile-command` is run before `swift-run-command`."
  (interactive)
  (swift-kill-async-buffer)
  (if current-prefix-arg
      (progn
        (add-to-list
         'swift-compilation-finished-functions
         (function
          (let ((dir
                 (expand-file-name
                  (locate-dominating-file
                   "." (lambda (parent)
                         (directory-files parent nil "Package.swift"))))))
            (async-shell-command (concat "cd '" dir "' && " swift-run-command) ))))
        (swift-compile))
    (let ((dir
           (expand-file-name
            (locate-dominating-file
             "." (lambda (parent)
                   (directory-files parent nil "Package.swift"))))))
      (async-shell-command (concat "cd '" dir "' && " swift-run-command) ))

    ))

(defun swift-compile ()
  "Compile swift project."
  (interactive)
  (let ((dir
         (expand-file-name
          (locate-dominating-file
           "." (lambda (parent)
                 (directory-files parent nil "Package.swift"))))))
    (compile (concat "cd '" dir "' && " "make -k"))))


(add-hook 'swift-mode-hook
          (lambda()
            (define-key swift-mode-map (kbd "C-c C-p") 'swift-mode:beginning-of-defun)
            (define-key swift-mode-map (kbd "C-c C-n") 'swift-mode:end-of-defun)
            (define-key swift-mode-map (kbd "M-RET") 'lsp-execute-code-action)
            (define-key swift-mode-map (kbd "C-c C-c") 'swift-compile)
            (define-key swift-mode-map (kbd "C-c C-r") 'swift-run)
            (define-key swift-mode-map (kbd "C-c C-t") 'swift-test)
            (setq compile-command "swift build")
            (setq swift-run-command "swift run")
            (setq compilation-read-command nil)
            (swift-extra-font-lock)
            (add-hook 'compilation-finish-functions 'swift-compilation-finished)
            (setq tramp-remote-path (append (list my-xctoolchain) tramp-remote-path))
            (lsp-register-client
             (make-lsp-client :new-connection (lsp-tramp-connection "sourcekit-lsp")
                              :major-modes '(swift-mode)
                              :remote? t
                              :server-id 'sourcekit-remote))
            (lsp)))

(setq swift-mode:parenthesized-expression-offset 4)
(setq swift-mode:multiline-statement-offset 4)

(defun swift-test()
  (interactive)
  (shell-command "set -o pipefail; swift test | xcbeautify")
  )

(defun swift-extra-font-lock()
  (interactive)
  ;; Type declarations
  (font-lock-add-keywords
   nil `((,(rx-to-string
            `(and bow upper (group (* (or word "<" ">" "?"))))
            t)
          0 font-lock-type-face)) t)

  ;; Matches higher order type declaration
  (font-lock-add-keywords
   nil `((,(rx-to-string
            `(and bow upper (group (* word) "<"))
            t)
          0 font-lock-type-face)) t)

  )

(defun ob-swift--eval (body)
  "Evaluate BODY with swift."
  (with-temp-buffer
    (insert body)
    (let*
        ((sources (concat (getenv "HOME") "/src"))
         (swift-libraries
          (list
           "swift-identified-collections"
           "swift-collections"
           "swift-algorithms"))
         (ob-swift-include-paths
          (mapcar (lambda (lib) (format "%s/%s/.build/debug" sources lib))
                  swift-libraries))
         (ob-swift-library-paths
          (mapcar (lambda (lib) (format "%s/%s/.build/debug" sources lib))
                  swift-libraries))
         (ob-swift-libraries
          (mapcar (lambda (lib) (format "%s__REPL" lib)) swift-libraries))
         (include-paths
          (if (> (length ob-swift-include-paths) 0)
              (concat
               " -I"
               (mapconcat
                'identity
                (cl-remove-if-not
                 (lambda(path) (and (file-exists-p path)
                                    (not (string-blank-p path)))) ob-swift-include-paths) " -I"))
            ""
            ))
         (library-paths
          (if (> (length ob-swift-library-paths) 0)
              (concat
               " -L"
               (mapconcat
                'identity
                (cl-remove-if-not
                 (lambda(path) (and (file-exists-p path)
                                    (not (string-blank-p path)))) ob-swift-library-paths) " -L"))
            ""
            ))
         (libraries
          (if (> (length ob-swift-libraries) 0)
              (concat
               " -l"
               (mapconcat
                'identity
                (cl-remove-if-not
                 (lambda (lib) (not (string-blank-p lib)))  ob-swift-libraries) " -l"))
            ""
            ))
         (swift-cmd (concat "swift" include-paths library-paths libraries " - ")))
      (progn
        (print swift-cmd)
        (shell-command-on-region (point-min) (point-max) swift-cmd nil 't)))
    (buffer-string)))

(provide 'my-swift)

;;; my-swift.el ends here
