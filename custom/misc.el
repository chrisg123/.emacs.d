;;; misc.el --- Miscellaneous Emacs settings
;;; commentary:
;; A place for miscellaneous Emacs settings.

;;; Code:

;; display the current column number
(setq column-number-mode t)

(global-flycheck-mode)

(semantic-mode 1)

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
;;(defvar visual-basic-ide-pathname)
;;(setq visual-basic-ide-pathname "/bin/bash -c VB.exe")
(add-hook 'visual-basic-mode-hook
          (lambda()
            (setq visual-basic-mode-indent 4)
            (setq-default indent-tabs-mode nil)
            (define-key visual-basic-mode-map (kbd "C-c w")
              (lambda() (interactive) (print "No whitespace cleanup for vb6 silly.")))
          ))

(elpy-enable)
(setq elpy-rpc-backend "jedi")
(setq eldoc-idle-delay 1)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(show-paren-mode t)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)



;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (previous-line 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (next-line 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

(defun add-include-path (path &optional buffer)
  "Add PATH to flycheck and semantic include-path.  Optional BUFFER."
  (interactive
   (list (string-trim-right (read-directory-name "Include path: "))))
  (if (file-directory-p path)
      (if (y-or-n-p (format "Path: `%s'\nAdd path to includes? " path))
          (progn
            (message (format "buffer: %s" buffer))
            (add-to-list 'flycheck-gcc-include-path path)
            (semantic-add-system-include path)
            (if buffer (switch-to-buffer buffer))
            (flycheck-buffer)
        )
        (message "Canceled."))
    (message "Path is not a directory.")))

(defun extra-include(path)
  "Load includes from `.extra_include' file in PATH."
  (interactive
   (list (string-trim-right (read-directory-name "Path to `.extra_include' parent directory: "))))
  (let ((xfile (format "%s/.extra_include" (replace-regexp-in-string "/*\s*$" "" path)))
        (buffer (current-buffer)))
    (if (file-exists-p xfile)
        (progn
          (message "file exists")
          (with-temp-buffer
            (insert-file-contents xfile)
            (mapcar (lambda (x) (add-include-path x buffer)) (split-string (buffer-string) "\n" t))
            )
          )
      (message (format "File not found: `%s'" xfile)))))

(provide 'misc)
;;; misc.el ends here
