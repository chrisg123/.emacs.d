;;; my-org.el --- Emacs auto save settings
;;; Commentary:
;;; Code:
(require 'org)
(require 'ox)
(require 'ob-kotlin)

(remove-hook 'org-mode-hook 'org-babel-hide-all-hashes)

(defvar org-log-done)
(setq org-log-done 'time)

(defvar org-hide-emphasis-markers)
(setq org-hide-emphasis-markers t)

(defvar org-edit-src-content-indentation)
(setq org-edit-src-content-indentation 0)
(setq org-startup-folded t)
(defun org-mode-keybinding()
  "Setup keybinding for 'org-mode'."
)

(defun oeg(property)
  "Get value of PROPERTY.  Shorthand for org-entry-get."
  (org-entry-get nil property t)
)

(defun pfmt (string &rest properties)
  "Format a STRING out of a format-string and org PROPERTIES.
Example usage: (pfmt \"/my/dir/path/%s.\" \"PROP\")"
  (apply 'format
         (append (list string)
                 (mapcar (lambda(x) (org-entry-get nil x t)) properties))))
(defun fdt()
  "Get a datetime string suitable for prefix or suffix of a filename."
  (format-time-string "%Y-%m-%d_%H_%M_%S" (current-time)))

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

(add-to-list 'org-export-filter-final-output-functions 'org-export-ascii-replace-nbs)
;;(add-hook 'org-export-before-parsing-hook #'org-export-ascii-replace-nbs)

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
             (delete '("\\.webp\\'" . default) org-file-apps)
             (delete '("\\.bmp\\'" . default) org-file-apps)
             (delete '("\\.mtl\\'" . default) org-file-apps)
             (add-to-list 'org-file-apps '(directory . emacs))
             (add-to-list 'org-file-apps '("\\.pdf\\'" . "mupdf %s"))
             (add-to-list 'org-file-apps '("\\.jpg\\'" . "feh -. %s"))
             (add-to-list 'org-file-apps '("\\.png\\'" . "feh -. %s"))
             (add-to-list 'org-file-apps '("\\.webp\\'" . "feh -. %s"))
             (add-to-list 'org-file-apps '("\\.bmp\\'" . "feh -. %s"))
             (add-to-list 'org-file-apps '("\\.mlt\\'" . "shotcut %s"))
             (add-to-list 'org-file-apps '("\\.mp3\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.m4a\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.webm\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.opus\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.oga\\'" . "mplayer -novideo %s"))
             (add-to-list 'org-file-apps '("\\.mpeg\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.mkv\\'" . "mplayer %s"))
             (add-to-list 'org-file-apps '("\\.htm\\'" . "firefox %s"))
             (add-to-list 'org-file-apps '("\\.html\\'" . "firefox %s"))
             ))

(add-hook 'org-src-mode-hook
          '(lambda()
             (global-display-line-numbers-mode 1)))

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

(defun org-insert-source-code-block(&optional language file)
  "Insert source code block for LANGUAGE.  Optionally pull in FILE contents.
Will prompt for LANGUAGE when called interactively.
With a `\\[universal-argument]' prefix, prompts for FILE.
The `:tangle FILE` header argument will be added when pulling in file contents."
  (interactive)
  (let ((col (current-column))
        (lang (or language (read-from-minibuffer "Source block language: ") ))
        (file (if current-prefix-arg (read-file-name "Find file: ") nil)))
    (insert
     (format "#+begin_src %s%s" lang (if file (concat " :tangle " file) "")))
    (newline)(newline)
    (move-to-column col t)(insert "#+end_src")(newline)
    (forward-line -2)(move-to-column col t)
    (if file (insert-file-contents file))))

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
   (swift . t)
   (perl . t)
   (kotlin . t)
   (js . t)
))

;; (defun org-babel-execute:vbnet (body params)
;;   "Execute a block of VB.NET code with Org-Babel."
;;   (let* ((wrapped-body (if (or (string-match "Module" body)
;;                                (string-match "Class" body))
;;                            body
;;                          (concat "Module Program\n    Sub Main()\n" body "\n    End Sub\nEnd Module")))
;;          (tmp-src-file (org-babel-temp-file "vbnet-src-" ".vb"))
;;          (tmp-out-file (org-babel-temp-file "vbnet-out-" ".exe")))
;;     (with-temp-file tmp-src-file
;;       (insert wrapped-body))
;;     (org-babel-eval
;;      (format "dotnet new console -lang vb -o temp-vbnet-project --force && cd temp-vbnet-project && mv %s Program.vb && dotnet run > %s"
;;              (shell-quote-argument tmp-src-file)
;;              (shell-quote-argument tmp-out-file))
;;      "")
;;     (with-temp-buffer
;;       (insert-file-contents tmp-out-file)
;;       (buffer-string))))

(defun org-babel-execute:vbnet (body params)
  "Execute a block of VB.NET code with Org-Babel."
  (let* ((lines (split-string body "\n" t))
         (imports '())
         (rest-lines '())
         (collecting-imports t)
         (wrapped-body))
    (if (or (string-match-p "\\<Module\\>" body)
            (string-match-p "\\<Class\\>" body))
        (setq wrapped-body body)
      (dolist (line lines)
        (if (and collecting-imports (string-match-p "^\\s-*Imports\\s-" line))
            (push line imports)
          (setq collecting-imports nil)
          (push line rest-lines)))
      (setq imports (reverse imports))
      (setq rest-lines (reverse rest-lines))
      (setq wrapped-body
            (concat
             (mapconcat 'identity imports "\n")
             (if imports "\n" "")
             "Module Program\n    Sub Main()\n"
             (mapconcat (lambda (line) (concat "        " line)) rest-lines "\n")
             "\n    End Sub\nEnd Module")))
    (let* ((tmp-src-file (org-babel-temp-file "vbnet-src-" ".vb"))
           (tmp-out-file (org-babel-temp-file "vbnet-out-" ".txt")))
      (with-temp-file tmp-src-file
        (insert wrapped-body))
      (org-babel-eval
       (format "dotnet new console -lang vb -o temp-vbnet-project --force && cd temp-vbnet-project && mv %s Program.vb && dotnet run > %s"
               (shell-quote-argument tmp-src-file)
               (shell-quote-argument tmp-out-file))
       "")
      (with-temp-buffer
        (insert-file-contents tmp-out-file)
        (buffer-string)))))


(add-to-list 'org-babel-tangle-lang-exts '("vbnet" . "vb"))

(setq org-ditaa-jar-path "/usr/share/ditaa/lib/ditaa.jar")

(defun org-babel-noweb-wrap (&optional regexp)
  "Return regexp matching a Noweb reference.

Match any reference, or only those matching REGEXP, if non-nil.

When matching, reference is stored in match group 1."
  (rx-to-string
   `(and (or "<<" "«")
           (group
            (not (or " " "\t" "\n"))
            (? (*? any) (not (or " " "\t" "\n"))))
           (or ">>" "»"))))
(provide 'my-org)

(defun ob-kotlin-eval-in-repl (session body)
  (let ((name (format "*ob-kotlin-%s*" session)))
    (setq ob-kotlin-process-output "")
    (process-send-string name (format "%s\n\"%s\"\n" body ob-kotlin-eoe))
    (accept-process-output (get-process name) nil nil 1)
    (ob-kotlin--wait ob-kotlin-eoe)
    (string-trim-right
     (replace-regexp-in-string
      "^res[0-9]*: " ""
      (replace-regexp-in-string
       (format "^>>>.*%s.*\n>>>" ob-kotlin-eoe) "" ob-kotlin-process-output)))))

(setq org-export-with-sub-superscripts nil)

(setq org-src-preserve-indentation t)

(defun render-openai-json ()
  "Render all thread.messages containing LaTeX equations in a browser using Google Chrome."
  (interactive)
  (let* ((json (org-babel-read-result))
         (parsed-json (json-read-from-string json))
         (messages (reverse (alist-get 'data parsed-json)))  ;; Reverse the list of messages
         (formatted-messages
          (mapconcat
           (lambda (message)
             (let ((role (alist-get 'role message))
                   (content (alist-get 'content message)))
               (if content
                   (concat "<p><strong>(" role ")</strong></p>"
                           (mapconcat
                            (lambda (item)
                              (replace-regexp-in-string "\n" "<br>" (alist-get 'value (alist-get 'text item))))
                            content
                            "<br><br>"))
                 "")))
           messages
           "<hr>"))
         (html-content (concat
                        "<!DOCTYPE html>\n"
                        "<html lang=\"en\">\n"
                        "<head>\n"
                        "    <meta charset=\"UTF-8\">\n"
                        "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
                        "    <title>Math Rendering</title>\n"
                        "    <script src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/3.2.2/es5/tex-mml-chtml.js\"></script>\n"
                        "    <style>\n"
                        "        body {\n"
                        "            font-family: Arial, sans-serif;\n"
                        "            line-height: 1.6;\n"
                        "            margin: 40px;\n"
                        "            background-color: #121212; /* Dark background */\n"
                        "            color: #e0e0e0; /* Light text */\n"
                        "        }\n"
                        "        #content {\n"
                        "            text-align: left;\n"
                        "            max-width: 800px;\n"
                        "            margin: auto;\n"
                        "        }\n"
                        "        .MathJax_Display {\n"
                        "            text-align: left !important;\n"
                        "        }\n"
                        "        a {\n"
                        "            color: #bb86fc; /* Light purple for links */\n"
                        "        }\n"
                        "    </style>\n"
                        "</head>\n"
                        "<body>\n"
                        "    <div id=\"content\">" formatted-messages "</div>\n"
                        "    <script>\n"
                        "        MathJax.typesetPromise();\n"
                        "    </script>\n"
                        "</body>\n"
                        "</html>\n"))
         (file-path "/tmp/openai-rendered.html"))
    (with-temp-file file-path
      (insert html-content))
    (start-process-shell-command "open-html" nil "google-chrome-stable /tmp/openai-rendered.html")))

(defun export-org-ascii-subtrees ()
  "Export each level‑1 subtree to its own ASCII file.
Default: use the subtree heading as the title (via `org-export-to-file`).
If a subtree has PROPERTY `EXPORT_DOC_TITLE`, use the global #+TITLE:
instead (via `org-export-as`).  Filenames come from
`EXPORT_FILE_NAME` or a slug of the heading (no extension).
Echo each export in the minibuffer."
  (interactive)
  ;; 1. Grab the global document title
  (let* ((env       (org-export-get-environment 'ascii nil nil))
         (doc-title (or (plist-get (cadr env) :title)
                        (file-name-base (buffer-file-name)))))
    ;; 2. Iterate over all level‑1 headings
    (org-map-entries
     (lambda ()
       (let* ((heading       (nth 4 (org-heading-components)))
              (slug          (replace-regexp-in-string "[[:space:]]+" "_" heading))
              (fname         (or (org-entry-get (point) "EXPORT_FILE_NAME")
                                 slug))
              (use-doc-title (org-entry-get (point) "EXPORT_DOC_TITLE")))
         (if use-doc-title
             ;; — Use global #+TITLE: via org-export-as
             (save-restriction
               (org-narrow-to-subtree)
               (let ((out-str
                      (org-export-as
                       'ascii    ; backend
                       nil       ; async?
                       nil       ; subtreep? (we've already narrowed)
                       nil       ; visible-only?
                       (list :title doc-title))))  ; ext-plist
                 (with-temp-file fname
                   (insert out-str))
                 (message "Exported (doc title) '%s' → %s" heading fname)))
           ;; — Default: subtree heading as title via org-export-to-file
           (progn
             (org-export-to-file
              'ascii   ; backend
              fname    ; output file
              nil      ; async?
              t        ; subtree-only
              nil      ; visible-only?
              nil)     ; ext-plist
             (message "Exported (subtree) '%s' → %s" heading fname))))))
     "LEVEL=1"  ; match only top‑level subtrees
     'file))   ; scope: current file


(setq org-latex-compiler "xelatex"
      org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

(with-eval-after-load 'ox-latex
  ;; 1) Register a new class "myarticle"
  (add-to-list 'org-latex-classes
               '("myarticle"
                 ;; Preamble:
                 "\\documentclass[11pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{graphicx}
\\usepackage[a4paper, margin=1in]{geometry}
\\usepackage{titling}
\\setlength{\\droptitle}{-2cm}
\\pretitle{\\vspace*{-2ex}\\centering\\LARGE\\bfseries}
\\posttitle{\\par\\vspace*{-2.5cm}}
\\setlength{\\parindent}{0pt}"
                 ;; How to translate Org sections -> LaTeX
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  ;; 2) Make "myarticle" the default class for all exports
  (setq org-latex-default-class "myarticle"))



;; \\usepackage{titlesec}
;; \\titlespacing*{\\section}{0pt}{0.5\\baselineskip}{0.5\\baselineskip}
;; \\titlespacing*{\\subsection}{0pt}{0.25\\baselineskip}{0.25\\baselineskip}

;;; my-org.el ends here
