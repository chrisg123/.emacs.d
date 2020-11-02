;;; xclip.el --- use xclip to copy/paste from emacs-nox
;;; Commentary:
;;; Code:

(unless window-system
  (when (getenv "DISPLAY")
    (defun xclip-cut-function (text &optional push)
      (with-temp-buffer
	(insert text)
	(call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))
    (defun xclip-paste-function()
      (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
	(unless (string= (car kill-ring) xclip-output)
	  xclip-output )))
    (defun clip-exe-cut-function (text &optional push)
      (with-temp-buffer
	(insert text)
	(call-process-region (point-min) (point-max) "clip.exe")))
    (defun clip-exe-paste-function()
      (let ((xclip-output (shell-command-to-string "powershell.exe -command Get-Clipboard | sed 's/\r//g'")))
	(unless (string= (car kill-ring) xclip-output)
	  xclip-output )))

    (if (string-match "-[Mm]icrosoft" operating-system-release)
	(setq interprogram-cut-function 'clip-exe-cut-function)
      (progn (setq interprogram-cut-function 'xclip-cut-function)
	     (setq interprogram-paste-function 'xclip-paste-function)))
    ))

(provide 'xclip)
;;; xclip.el ends here
