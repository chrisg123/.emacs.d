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

(add-hook 'swift-mode-hook
  (lambda()
    (define-key swift-mode-map (kbd "C-c C-p") 'swift-mode:beginning-of-defun)
    (define-key swift-mode-map (kbd "C-c C-n") 'swift-mode:end-of-defun)
    (setq tramp-remote-path (append (list my-xctoolchain) tramp-remote-path))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "sourcekit-lsp")
                      :major-modes '(swift-mode)
                      :remote? t
                      :server-id 'sourcekit-remote))
    (lsp)))

(provide 'my-swift)

;;; my-swift.el ends here
