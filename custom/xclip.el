;;; xclip.el --- use xclip to copy/paste from emacs-nox
;;; Commentary:
;;; Code:

(defun xsel-cut-function(text &optional push)
  "Copy TEXT to clipboard with xsel.  Optional PUSH."
  (with-temp-buffer
    (call-process-region text nil "xsel" nil t nil "-bi")
    (buffer-string)))

(defun xsel-paste-function()
  "Paste from clipboard with xsel."
  (let (xsel-output (shell-command-to-string "xsel -o"))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output)))

(defun clip-exe-cut-function (text &optional push)
  "Copy TEXT to clipboard with clip.exe.  Optional PUSH."
  (with-temp-buffer
    (insert text)
    (call-process-region
     text nil "/mnt/c/Windows/System32/clip.exe" nil t nil)))

(defun clip-exe-paste-function()
  "Pase from clipboard with clip.exe."
  (let ((xclip-output
         (shell-command-to-string
          "/mnt/c/Windows/system32/WindowsPowerShell/v1.0/powershell.exe -command Get-Clipboard 2> /dev/null | sed 's/\r//g'")))
    (unless (string= (car kill-ring) xclip-output)
      xclip-output )))

(unless window-system
  (when (getenv "DISPLAY")

    (cond

     ((string-match "-[Mm]icrosoft" operating-system-release)
      (progn
        (setq interprogram-cut-function 'clip-exe-cut-function)
        ;;(setq interprogram-paste-function 'clip-exe-paste-function)
        ))

     ((eq system-type 'darwin)
      (progn (princ "TODO: darwin")))

     ((eq system-type 'gnu/linux)
      (progn
        (setq interprogram-cut-function 'xsel-cut-function)
        (setq interprogram-paste-function 'xsel-paste-function)
        ))
     )))


;; (unless window-system
;;   (when (getenv "DISPLAY")

;; (defun xclip-cut-function (text &optional push)
;;   (with-temp-buffer
;;     (insert text)
;;     (let (;; prevent tramp from calling remotely
;;           (default-directory (getenv "HOME")))
;;       (call-process-region
;;        (point-min)
;;        (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")
;;       )
;;     ))

;; (defun xclip-paste-function()
;;   (let (;; prevent tramp from calling remotely
;;         (default-directory (getenv "HOME")))
;;     (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
;;       (unless (string= (car kill-ring) xclip-output)
;;         xclip-output))
;;     )
;;   )

;; (defun clip-exe-cut-function (text &optional push)
;;   (with-temp-buffer
;;     (insert text)
;;      (call-process-region (point-min) (point-max) "/mnt/c/Windows/system32/clip.exe")))

;; (defun clip-exe-paste-function()
;;   (let ((xclip-output (shell-command-to-string "powershell.exe -command Get-Clipboard | sed 's/\r//g'")))
;;     (unless (string= (car kill-ring) xclip-output)
;;       xclip-output )))

;; (cond
;;  ((string-match "-[Mm]icrosoft" operating-system-release)
;;   (setq interprogram-cut-function 'clip-exe-cut-function))

;;  ((eq system-type 'darwin) nil)

;;  ((eq system-type 'gnu/linux)
;;   (progn (setq interprogram-cut-function 'xclip-cut-function)
;;          (setq interprogram-paste-function 'xclip-paste-function))
;;   ))

;; )

;; )

(provide 'xclip)
;;; xclip.el ends here
