;;; my-org.el --- Emacs auto save settings
;;; Commentary:
;;; Code:

(require 'ob-async)
(require 'ox)
(remove-hook 'org-mode-hook 'org-babel-hide-all-hashes)

(defvar org-log-done)
(setq org-log-done 'time)

(defvar org-hide-emphasis-markers)
(setq org-hide-emphasis-markers t)

(defvar org-edit-src-content-indentation)
(setq org-edit-src-content-indentation 0)

(defun org-mode-keybinding()
  "Setup keybinding for 'org-mode'."
)

(defun oeg(property)
  "Get value of PROPERTY.  Shorthand for org-entry-get."
  (org-entry-get nil property t)
)
(defun org-babel-get-result (name)
  "Get results at NAME into emacs-lisp."
  (save-excursion
    (org-babel-goto-named-result name)
    (org-babel-read-result)))

(defun org-export-ascii-replace-nbs (text backend info)
  "Replace non-breaking-space with regular space for ascii BACKEND."
  (when (org-export-derived-backend-p backend 'ascii)
    (let ((nbs (char-to-string #xa0)) (spc (char-to-string #x20)))
          (replace-regexp-in-string nbs spc text))))

(defun org-babel-color-results()
  "Color org babel results.
Use the :wrap header argument on your code block for best results.
Without it, org may color overtop with whatever default coloring it
uses for literal examples.
Depends on the xterm-color package."
  (interactive)
  (when-let ((result-start (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char result-start)
      (when (looking-at org-babel-result-regexp)
        (let ((element (org-element-at-point)))
          (pcase (org-element-type element)
            (`fixed-width
             (let ((post-affiliated (org-element-property :post-affiliated element))
                   (end (org-element-property :end element))
                   (contents (org-element-property :value element))
                   (post-blank (org-element-property :post-blank element)))
               (goto-char post-affiliated)
               (delete-region post-affiliated end)
               (insert (xterm-color-filter contents))
               (org-babel-examplify-region post-affiliated (point))
               (insert (make-string post-blank ?\n ))
               ))
            ((or `center-block `quote-block `verse-block `special-block)
             (let ((contents-begin (org-element-property :contents-begin element))
                   (contents-end (org-element-property :contents-end element)))
               (goto-char contents-begin)
               (let ((contents (buffer-substring contents-begin contents-end)))
                 (delete-region contents-begin contents-end)
                 (insert (xterm-color-filter contents)))))))))))

(add-hook 'org-babel-after-execute-hook 'org-babel-color-results)

(add-to-list 'org-export-filter-final-output-functions 'org-export-ascii-replace-nbs)
;;(add-hook 'org-export-before-parsing-hook #'org-export-ascii-replace-nbs)

(add-hook 'org-mode-hook
          (lambda()
            (org-indent-mode)
            (org-mode-keybinding)
            ))

;; PDFs visited in Org-mode are opened in Evince (and not in the default choice)
;; https://stackoverflow.com/a/8836108/2974621
;; https://stackoverflow.com/a/8836108/789593
(add-hook 'org-mode-hook
          '(lambda ()
             (delete '("\\.pdf\\'" . default) org-file-apps)
             (delete '("\\.jpg\\'" . default) org-file-apps)
             (delete '("\\.png\\'" . default) org-file-apps)
             (delete '("\\.bmp\\'" . default) org-file-apps)
             (delete '("\\.mtl\\'" . default) org-file-apps)
             (add-to-list 'org-file-apps '(directory . emacs))
             (add-to-list 'org-file-apps '("\\.pdf\\'" . "mupdf %s"))
             (add-to-list 'org-file-apps '("\\.jpg\\'" . "feh -. %s"))
             (add-to-list 'org-file-apps '("\\.png\\'" . "feh -. %s"))
             (add-to-list 'org-file-apps '("\\.bmp\\'" . "feh -. %s"))
             (add-to-list 'org-file-apps '("\\.mlt\\'" . "shotcut %s"))
             (add-to-list 'org-file-apps '("\\.mp3\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.m4a\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.webm\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.mpeg\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.mkv\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.htm\\'" . "firefox %s"))
             (add-to-list 'org-file-apps '("\\.html\\'" . "firefox %s"))
             ))

(add-hook 'org-src-mode-hook
          '(lambda() (global-display-line-numbers-mode 1)))

(defun escape-src-block-tags(str)
  "Escape source code declaration block tags in STR."
  (let ((a (replace-regexp-in-string "|" "\\\\|" str)))
    (replace-regexp-in-string "\\+" "\\\\+" a))
)

(defvar src-block-tags-regexp
  (escape-src-block-tags
   (string-join
    '("#+BEGIN_SRC"
      "#+END_SRC"
      "#+begin_"
      "#+end_"
      "#+HEADER"
      "#+name"
      "#+RESULTS"
      "#+BEGIN_QUOTE"
      "#+END_QUOTE"
      )"|")))

(defun insert-source-block()
  "Insert source code declaration block."
  (interactive)
  (let ((col (current-column))(input (read-from-minibuffer "language: ")))
    (insert (format "#+BEGIN_SRC %s" input))(newline)(newline)
    (move-to-column col t)(insert "#+END_SRC")(newline)
    (forward-line -2)(move-to-column col t)))

(defvar src-block-overlays (list))

(defun hide-source-block-delimeters()
  "Hide source block delimiters."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (let ((case-fold-search t))
      (while (re-search-backward src-block-tags-regexp nil t)
        (if (string-blank-p (buffer-substring-no-properties
                             (line-beginning-position) (point)))
            (progn
              (remove-overlays (line-beginning-position) (line-end-position))
              (let ((ov-src-block-delim
                     (make-overlay
                      (line-beginning-position) (+ 1 (line-end-position)))))
                (overlay-put ov-src-block-delim 'src-block-delim t)
                (overlay-put ov-src-block-delim 'invisible t)
                (push ov-src-block-delim src-block-overlays)
                )))))))


(defun unhide-source-block-delimiters()
  "Unhide source block delimiters."
  (interactive)
  (delete-overlays src-block-overlays)
  (org-babel-hide-all-hashes))

(defun delete-overlays (ovs)
  "Delete OVS."
  (if (overlayp (car ovs)) (delete-overlay (car ovs)))
  (if ovs (delete-overlays (cdr ovs))))

(defun insert-org-mode-header()
  "Insert org mode declaration header."
  (interactive)
  (insert "# -*- mode: org -*-")(newline)
  (let ((input (read-from-minibuffer "Title: ")))
    (insert (format "#+TITLE: %s" input)))(newline)
    (insert "#+DATE:")(newline)
    (insert "#+OPTIONS: toc:nil num:nil tags:nil")(newline)
    (insert "#+PROPERTY: header-args :eval never-export :results output")
  (end-of-line)(newline)(newline)
  (save-buffer t)
  (revert-buffer :ignore-auto :nonconfirm)
  )

(defvar org-bracket-link-regexp)

;; ref: https://emacs.stackexchange.com/a/3990
(defun org-kill-url ()
  "Extract url from org link."
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
     (text (when link-info
         ;; org-context seems to return nil if the current element
         ;; starts at buffer-start or ends at buffer-end
         (buffer-substring-no-properties (or (cadr link-info) (point-min))
                         (or (caddr link-info) (point-max))))))
    (if (not text)
    (error "Not in org link")
      (kill-new ((lambda (x)
            (string-match org-link-bracket-re x)
            (substring x (match-beginning 1) (match-end 1))) text)))))


(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   (ditaa . t)
))

(setq org-ditaa-jar-path "/usr/share/ditaa/lib/ditaa.jar")

(provide 'my-org)

;;; my-org.el ends here
