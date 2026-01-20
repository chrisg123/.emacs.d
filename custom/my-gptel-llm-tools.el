;;; my-gptel-llm-tools.el --- LLM-accessible gptel tools  -*- lexical-binding: t; -*-

;;; Commentary:
;; LLM-accessible tool surface area.  Keep tight and auditable.

;;; Code:

(require 'gptel)
(require 'subr-x)

;; ---------------------------------------------------------------------------
;; Security configuration (edit these)
;; ---------------------------------------------------------------------------

(defgroup my-gptel-llm-tools nil
  "Security settings for LLM-accessible gptel tools."
  :group 'tools)

(defcustom my-gptel-llm-tools-root (expand-file-name "~/.emacs.d/gptel-sandbox/")
  "Only paths under this directory are writable by LLM tools."
  :type 'directory)

(defcustom my-gptel-llm-tools-allowed-extensions '("org" "md" "txt" "el" "patch", "sh", "py", "js", "vb", "c", "cpp")
  "Allowed file extensions for LLM create/edit operations (no dot)."
  :type '(repeat string))

(defcustom my-gptel-llm-tools-max-bytes (* 256 1024)
  "Refuse to edit files larger than this many bytes."
  :type 'integer)

(defcustom my-gptel-llm-tools-deny-filenames
  '("init.el" "early-init.el" "custom.el"
    ".authinfo" ".authinfo.gpg")
  "Basename filenames that are always denied (even under root)."
  :type '(repeat string))

(defcustom my-gptel-llm-tools-deny-regexps
  (list
   (regexp-quote (expand-file-name "~/.ssh/"))
   (regexp-quote (expand-file-name "~/.gnupg/")))
  "Regexps matching absolute paths that are always denied."
  :type '(repeat regexp))

(defcustom my-gptel-llm-tools-audit-log-file
  (expand-file-name "llm-tools-audit.log" my-gptel-llm-tools-root)
  "Append-only audit log file for tool operations."
  :type 'file)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun my-gptel-llm-tools--root ()
  (file-name-as-directory (file-truename my-gptel-llm-tools-root)))

(defun my-gptel-llm-tools--audit (fmt &rest args)
  (let* ((root (my-gptel-llm-tools--root))
         (logfile (expand-file-name (file-name-nondirectory my-gptel-llm-tools-audit-log-file) root))
         (line (format "[%s] %s\n" (format-time-string "%Y-%m-%d %H:%M:%S%z")
                       (apply #'format fmt args))))
    (make-directory root t)
    (with-temp-buffer
      (insert line)
      (write-region (point-min) (point-max) logfile 'append 'silent))))

(defun my-gptel-llm-tools--deny-path-p (truename)
  (or (seq-some (lambda (re) (string-match-p re truename))
                my-gptel-llm-tools-deny-regexps)))

(defun my-gptel-llm-tools--allowed-extension-p (filename)
  (let ((ext (downcase (or (file-name-extension filename) ""))))
    (member ext my-gptel-llm-tools-allowed-extensions)))

(defun my-gptel-llm-tools--validate-target (path filename &optional must-exist)
  "Return absolute truename for PATH+FILENAME after enforcing security policy."
  (let* ((root (my-gptel-llm-tools--root))
         ;; Anchor PATH to ROOT unless PATH is already absolute.
         (base-dir (if (file-name-absolute-p path)
                       (expand-file-name path)
                     (expand-file-name path root)))
         (full (expand-file-name filename base-dir))
         (full-expanded (expand-file-name full))
         ;; Important: truename resolves symlinks; for non-existent targets, we
         ;; validate the parent directory truename and then append basename.
         (dir (file-name-directory full-expanded))
         (base (file-name-nondirectory full-expanded)))
    (when (or (string-empty-p base) (string= base ".") (string= base ".."))
      (error "Invalid filename: %S" filename))
    (when (member base my-gptel-llm-tools-deny-filenames)
      (error "Denied filename: %s" base))
    (unless (my-gptel-llm-tools--allowed-extension-p base)
      (error "Denied extension: %s (allowed: %s)"
             (or (file-name-extension base) "<none>")
             my-gptel-llm-tools-allowed-extensions))
    ;; Validate directory containment using truename.
    (unless (file-directory-p dir)
      (when must-exist (error "Directory does not exist: %s" dir))
      (make-directory dir t))
    (let* ((dir-true (file-name-as-directory (file-truename dir)))
           (target (expand-file-name base dir-true)))
      (unless (string-prefix-p root dir-true)
        (error "Denied: target not under sandbox root (%s)" root))
      (when (my-gptel-llm-tools--deny-path-p target)
        (error "Denied: sensitive path"))
      ;; Reject symlink targets (if it exists and is symlink).
      (when (and (file-exists-p target) (file-symlink-p target))
        (error "Denied: target is a symlink"))
      target)))

(defun my-gptel-llm-tools--validate-size (file)
  (when (and (file-exists-p file)
             (> (file-attribute-size (file-attributes file))
                my-gptel-llm-tools-max-bytes))
    (error "Denied: file too large (> %s bytes)" my-gptel-llm-tools-max-bytes)))

(defun my-gptel-llm-tools--strip-diff-git-header (patch)
  "Strip leading `diff --git ...' header lines from PATCH.

This is intentionally conservative: it only removes *leading* lines of the form
`diff --git ...` (possibly repeated). Everything else is left untouched so
unified diff content (---/+++/@@) remains identical.

Return the normalized patch string."
  (replace-regexp-in-string
   "\\`\\(?:diff --git .*\\(?:\n\\)\\)+"
   ""
   patch))

;; ---------------------------------------------------------------------------
;; Approved LLM tools (locked to sandbox)
;; ---------------------------------------------------------------------------

(gptel-make-tool
 :name "create_file"
 :function
 (lambda (path filename content)
   (let* ((target (my-gptel-llm-tools--validate-target path filename nil)))
     (my-gptel-llm-tools--audit "create_file %s bytes=%d" target (length content))
     (make-directory (file-name-directory target) t)
     (with-temp-buffer
       (insert content)
       (write-region (point-min) (point-max) target nil 'silent))
     (format "Created file: %s" target)))
 :description "Create a new file WITHIN the sandbox root with the specified content."
 :args (list
        '(:name "path" :type string :description "Directory (must be under sandbox root)")
        '(:name "filename" :type string :description "File name to create (restricted extensions)")
        '(:name "content" :type string :description "The content to write"))
 :category "filesystem")

(gptel-make-tool
 :name "apply_patch_to_file"
 :function
 (lambda (path filename patch)
   (let* ((target (my-gptel-llm-tools--validate-target path filename t)))
     (my-gptel-llm-tools--validate-size target)
     (my-gptel-llm-tools--audit "apply_patch_to_file %s patch_bytes=%d" target (length patch))
     ;; Use `patch` if available; safer/more standard than a hypothetical apply_patch.
     (unless (executable-find "patch")
       (error "No 'patch' executable found on PATH"))
     (let* ((buf (generate-new-buffer " *gptel-patch*"))
            (default-directory (file-name-directory target))
            ;; Use -p0 and explicit file. Patch should refer to just `filename`
            ;; (or we can enforce that in prompts).
            (patch-file (make-temp-file "gptel-" nil ".patch")))
       (unwind-protect
           (progn

             (let* ((is-git (string-match-p "^--- a/" patch))
                    (p-level (if is-git "-p1" "-p0"))
                    ;; Optional: drop `diff --git ...` lines; keep rest identical
                    (patch-text (my-gptel-llm-tools--strip-diff-git-header patch)))
               (with-temp-file patch-file (insert patch-text))
               ;; Dry run first
               (with-current-buffer buf
                 (let ((dry-exit
                        (call-process "patch" nil buf t
                                      "--batch" "--forward"
                                      "--dry-run"
                                      "-d" (file-name-directory target)
                                      p-level "-i" patch-file)))
                   (unless (and (integerp dry-exit) (= dry-exit 0))
                     (error "Patch dry-run failed:\n%s" (buffer-string)))))
               (with-current-buffer buf
                 (erase-buffer)
                 (let ((exit
                        (call-process "patch" nil buf t
                                      "--batch" "--forward"
                                      "-d" (file-name-directory target)
                                      p-level "-i" patch-file)))
                   (if (and (integerp exit) (= exit 0))
                       (format "Patched file: %s" target)
                     (error "Patch failed:\n%s" (buffer-string))))
                 )))
         (when (and patch-file (file-exists-p patch-file)) (delete-file patch-file))
         (when (buffer-live-p buf) (kill-buffer buf))))))
 :description "Apply a unified diff patch to an existing sandbox file (dry-run enforced)."
 :args (list
        '(:name "path" :type string :description "Directory (must be under sandbox root)")
        '(:name "filename" :type string :description "Existing file name (restricted extensions)")
        '(:name "patch" :type string :description "Unified diff patch text"))
 :category "filesystem")

(provide 'my-gptel-llm-tools)
;;; my-gptel-llm-tools.el ends here
