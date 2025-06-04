;;; my-xaml.el --- Xaml customization
;;; Commentary:

;;; Code:
(require 'vbnet-mode)
(require 'my-vbnet)

(defvar vbnet-run-command)

(defun xaml-project-vbnet-p ()
  "Return the directory containing a .vbproj file if it exists, nil otherwise."
  (locate-dominating-file default-directory
    (lambda (parent)
      (directory-files parent nil ".*\\.vbproj$"))))

(defun xaml-compile ()
  "Compile the current project as VB.NET if a .vbproj file is found.
If not, output an error message."
  (interactive)
  (if (xaml-project-vbnet-p)
      (progn
        (message "VB.NET project detected. Delegating to vbnet-compile.")
        (vbnet-compile))
    (message "Could not determine project type: No .vbproj file found.")))

(defun xaml-run ()
  "Run the current project as VB.NET if a .vbproj file is found.
If not, output an error message."
  (interactive)
  (if (xaml-project-vbnet-p)
      (progn
        (message "VB.NET project detected. Delegating to vbnet-run.")
        (vbnet-run))
    (message "Could not determine project type: No .vbproj file found.")))

;; Add keybindings to xml-mode (commonly used for XAML files)
(add-hook 'nxml-mode-hook
          (lambda ()
            ;; Bind compile and run hotkeys similar to vbnet-mode.
            (local-set-key (kbd "C-c C-c") 'xaml-compile)
            (local-set-key (kbd "C-c C-r") 'xaml-run)
            (local-set-key (kbd "C-c C-u") 'my-push-or-pull-files)
            ;; Ensure necessary commands are set. If your VB.NET settings are
            ;; buffer-local only in vbnet-mode, you might want to duplicate them.
            (setq compile-command "./build.sh")
            (setq vbnet-run-command "./build.sh -r")
            (setq compilation-read-command nil)
            ;; Optionally, if you have test running commands:
            ;; (local-set-key (kbd "C-c C-t") 'xaml-run-tests)
            ;; (setq vbnet-run-tests-command "./testrun.py")
            ))

(provide 'my-xaml)

;;; my-xaml.el ends here
