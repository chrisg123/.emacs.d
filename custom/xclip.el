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

(defun powershell-clipboard-paste ()
  "Paste text from the Windows clipboard using PowerShell."
  (let ((clip-output (string-trim
                      (shell-command-to-string
                       "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -Command Get-Clipboard"))))
    (unless (string= clip-output (car kill-ring))
      clip-output)))


(defun detect-os ()
  "Detect the operating system and return a symbol."
  (cond
   ((string-match "Microsoft" (shell-command-to-string "uname -r"))
    'wsl)                      ;; Running in WSL
   ((eq system-type 'windows-nt)
    'windows)                  ;; Running on native Windows
   ((eq system-type 'gnu/linux)
    'linux)                    ;; Running on Linux
   (t
    'other)))                  ;; Other OS

(defun toggle-clipboard ()
  "Toggle clipboard integration in Emacs."
  (interactive)
  (if (eq interprogram-cut-function nil)
      (progn
        ;; Restore clipboard functions based on the operating system
        (pcase (detect-os)
          ('wsl
           (setq interprogram-cut-function 'clip-exe-cut-function)
           (setq interprogram-paste-function 'powershell-clipboard-paste))
          ('windows
           (setq interprogram-cut-function 'clip-exe-cut-function)
           (setq interprogram-paste-function 'powershell-clipboard-paste))
          ('linux
           (setq interprogram-cut-function 'xsel-cut-function)
           (setq interprogram-paste-function 'xsel-paste-function))
          (_ (message "Clipboard integration not configured for this OS.")))
        (message "Clipboard integration enabled"))
    (progn
      ;; Disable clipboard functions
      (setq interprogram-cut-function nil)
      (setq interprogram-paste-function nil)
      (message "Clipboard integration disabled"))))


(defun enable-clipboard-integration ()
  "Enable clipboard integration based on the operating system."
  (pcase (detect-os)
    ('wsl
     ;; Do not enable clipboard integration on WSL by default
     (message "Running on WSL. Clipboard integration is disabled by default. Use C-u M-w to copy to system clipboard."))
    ('windows
     ;; Setup clipboard functions for native Windows
     (setq interprogram-cut-function 'clip-exe-cut-function)
     (setq interprogram-paste-function 'powershell-clipboard-paste)
     (message "Clipboard integration enabled for Windows."))
    ('linux
     ;; Setup clipboard functions for native Linux
     (setq interprogram-cut-function 'xsel-cut-function)
     (setq interprogram-paste-function 'xsel-paste-function)
     (message "Clipboard integration enabled for Linux."))
    (_ (message "Clipboard integration not configured for this OS."))))

(enable-clipboard-integration)

(defun my/clipboard‐after‐kill‐ring‐save (&rest args)
  "After `kill-ring-save`, if called with C-u on WSL also send to system clipboard."
  (when (and current-prefix-arg
             (eq (detect-os) 'wsl))
    (let ((beg (nth 0 args))
          (end (nth 1 args)))
      (clip-exe-cut-function
       (buffer-substring-no-properties beg end))
      (message "Copied region to kill-ring and system clipboard"))))

(advice-add 'kill-ring-save :after #'my/clipboard‐after‐kill‐ring‐save)

(defun my/clipboard‐after‐kill‐region (&rest args)
  "After `kill-region`, if called with C-u on WSL also send to system clipboard."
  (when (and current-prefix-arg
             (eq (detect-os) 'wsl))
    (let ((beg (nth 0 args))
          (end (nth 1 args)))
      (clip-exe-cut-function
       (buffer-substring-no-properties beg end))
      (message "Cut region to kill-ring and system clipboard"))))

(advice-add 'kill-region :after #'my/clipboard‐after‐kill‐region)

(defun my/clipboard‐around‐yank (orig-fun &rest args)
  "If called with C-u on WSL, yank from system clipboard; otherwise do normal `yank`."
  (if (and current-prefix-arg
           (eq (detect-os) 'wsl))
      (let ((txt (powershell-clipboard-paste)))
        (if txt
            (insert txt)
          (message "No clipboard content available.")))
    (apply orig-fun args)))

(advice-add 'yank :around #'my/clipboard‐around‐yank)

(provide 'xclip)
;;; xclip.el ends here
