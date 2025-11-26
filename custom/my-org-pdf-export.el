;;; my-org-pdf-export.el --- Exporting Org to PDF -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'org)
(require 'ox)

(defgroup my-org-pdf-export nil
  "Custom Org PDF export variants.
This group contains settings for exporting Org buffers to PDF using
various LaTeX classes."
  :group 'org
  :prefix "my-org-pdf-"
  :tag "My Org PDF Export")

(with-eval-after-load 'ox-latex
  (setq org-latex-src-block-backend 'minted
        ;; make sure TeX can call Pygments
        org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; default minted options: font size, line breaking, tab size
  (setq org-latex-minted-options
        '(("fontsize" "\\footnotesize")
          ("breaklines" "true")
          ("tabsize" "2")))

  (dolist (cls
           ;; each element is (NAME . DEFINITION)
           '(
             ("default-portrait"
              ;; Preamble + section mappings:
              "\\documentclass[11pt]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{fontspec}                % <-- fontspec for XeLaTeX
\\setmainfont{Arial}                  % <-- set the body font to Arial
\\usepackage{graphicx}
\\usepackage{minted}
\\usepackage{titlesec}
\\titlespacing*{\\section}{0pt}{0.5\\baselineskip}{0.5\\baselineskip}
\\titlespacing*{\\subsection}{0pt}{0.25\\baselineskip}{0.25\\baselineskip}
\\usepackage{titling}
\\setlength{\\droptitle}{-2cm}
\\pretitle{\\vspace*{-2ex}\\centering\\LARGE\\bfseries}
\\posttitle{\\par\\vspace*{-1em}}
\\setlength{\\parindent}{0pt}
\\usepackage{enumitem}%
\\setlist[description]{style=nextline,font=\\bfseries,leftmargin=1.5em,labelsep=0.5em}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
             ("default-landscape"
              "\\documentclass[11pt]{article}
\\usepackage[a4paper,landscape,margin=1in]{geometry}
\\usepackage{fontspec}                % <-- fontspec for XeLaTeX
\\setmainfont{Arial}                  % <-- set the body font to Arial
\\usepackage{graphicx}
\\usepackage{minted}
\\usepackage{titlesec}
\\titlespacing*{\\section}{0pt}{0.5\\baselineskip}{0.5\\baselineskip}
\\titlespacing*{\\subsection}{0pt}{0.25\\baselineskip}{0.25\\baselineskip}
\\usepackage{titling}
\\setlength{\\droptitle}{-2cm}
\\pretitle{\\vspace*{-2ex}\\centering\\LARGE\\bfseries}
\\posttitle{\\par\\vspace*{-1em}}
\\setlength{\\parindent}{0pt}
\\usepackage{enumitem}%
\\setlist[description]{style=nextline,font=\\bfseries,leftmargin=1.5em,labelsep=0.5em}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
    (add-to-list 'org-latex-classes cls)))

(defcustom my-org-pdf-export-variants
  '((portrait . "default-portrait")
    (landscape . "default-landscape"))
  "Alist mapping export VARIANT symbols to their LaTeX CLASS names.
Each element is a cons cell (VARIANT . CLASS-NAME) where VARIANT is a symbol
and CLASS-NAME is a string naming the LaTeX class to use."
  :type '(alist :key-type symbol :value-type string)
  :group 'my-org-pdf-export)

(defun my-org-pdf-export (variant &optional file)
  "Export the current Org buffer to PDF using the VARIANT layout.
VARIANT is a symbol key in `my-org-pdf-export-variants'.  Optional argument
FILE (a string) specifies the output filename."
  (interactive
   (list (intern (completing-read "Export variant: "
                                  (mapcar #'car my-org-pdf-export-variants)))
         (when current-prefix-arg
           (read-file-name "Output PDF file: "))))
  (let ((org-latex-default-class
         (cdr (assoc variant my-org-pdf-export-variants))))
    (if file
        (org-latex-export-to-pdf
         nil nil nil nil
         `(:output-file ,file))
      (org-latex-export-to-pdf))))

(defun my-org-pdf-export-portrait (&optional file)
  "Export the current Org buffer to PDF using the portrait layout.
Optional argument FILE (a string) specifies the output filename."
  (interactive)
  (my-org-pdf-export 'portrait file))

(defun my-org-pdf-export-landscape (&optional file)
  "Export the current Org buffer to PDF using the landscape layout.
Optional argument FILE (a string) specifies the output filename."
  (interactive)
  (my-org-pdf-export 'landscape file))

(provide 'my-org-pdf-export)
;;; my-org-pdf-export.el ends here
