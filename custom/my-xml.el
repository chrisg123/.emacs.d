;;; my-xml.el --- XML customization
;;; Commentary:

;;; Code:
(require 'nxml-mode)
(require 'xml)
(require 'subr-x)
(require 'format-all)

(defvar-local my-xml-attribute-editor-source-buffer nil)
(defvar-local my-xml-attribute-editor-source-start nil)
(defvar-local my-xml-attribute-editor-source-end nil)
(defvar-local my-xml-attribute-editor-element-name nil)
(defvar-local my-xml-attribute-editor-self-closing nil)
(defvar-local my-xml-attribute-editor-original-tag nil)
(defvar-local my-xml-attribute-editor-window-configuration nil)


(defun my-xml-tag-end (start)
  "Return the position after the closing > of the tag at START."
  (save-excursion
    (goto-char start)
    (let ((quote nil)
          end)
      (while (and (not end)
                  (< (point) (point-max)))
        (let ((char (char-after)))
          (cond
           (quote
            (when (= char quote)
              (setq quote nil)))

           ((or (= char ?\")
                (= char ?'))
            (setq quote char))

           ((= char ?>)
            (setq end (1+ (point))))))

        (unless end
          (forward-char 1)))

      end)))


(defun my-xml-tag-bounds ()
  "Return the bounds of the XML start tag associated with point."
  (let ((origin (point))
        bounds)

    ;; Check whether point is directly inside a start tag.
    (save-excursion
      (goto-char (min (point-max)
                      (1+ origin)))

      (while (and (not bounds)
                  (search-backward "<" nil t))
        (let* ((start (point))
               (next (char-after (1+ start))))

          ;; Ignore closing tags, comments, declarations, etc.
          (unless (memq next '(?/ ?! ??))
            (let ((end (my-xml-tag-end start)))
              (when (and end
                         (<= start origin)
                         (<= origin end))
                (setq bounds
                      (cons start end))))))))

    ;; Otherwise find the start tag of the containing element.
    (or bounds
        (save-excursion
          (goto-char origin)

          (condition-case nil
              (progn
                (nxml-backward-up-element)

                (let ((start (point)))
                  (when (= (char-after start) ?<)
                    (let ((end (my-xml-tag-end start)))
                      (when end
                        (cons start end))))))

            (error nil))))))


(defun my-xml-parse-tag (start end)
  "Parse the XML start tag between START and END."
  (let* ((text (buffer-substring-no-properties start end))
         (length (length text))
         (position 0)
         element
         attributes
         self-closing)

    (unless (string-match
             "\\`<\\([^[:space:]/>]+\\)"
             text)
      (user-error "Could not determine element name"))

    (setq element
          (match-string 1 text))

    (setq position
          (match-end 0))

    (while (< position length)

      ;; Skip whitespace.
      (while (and (< position length)
                  (memq (aref text position)
                        '(?\s ?\t ?\n ?\r)))
        (setq position
              (1+ position)))

      (cond

       ;; End of normal tag.
       ((and (< position length)
             (= (aref text position) ?>))
        (setq position length))

       ;; End of self-closing tag.
       ((and (< (1+ position) length)
             (= (aref text position) ?/)
             (= (aref text (1+ position)) ?>))
        (setq self-closing t)
        (setq position length))

       ;; Parse attribute.
       (t
        (unless
            (and
             (string-match
              "\\([^[:space:]=/>]+\\)[[:space:]\n\r\t]*=[[:space:]\n\r\t]*\\([\"']\\)"
              text
              position)
             (= (match-beginning 0)
                position))

          (user-error
           "Could not parse attribute near: %s"
           (substring text position)))

        (let* ((name
                (match-string 1 text))

               (quote
                (aref text
                      (match-beginning 2)))

               (value-start
                (match-end 2))

               (value-end
                (string-match
                 (regexp-quote
                  (char-to-string quote))
                 text
                 value-start)))

          (unless value-end
            (user-error
             "Unterminated attribute %s"
             name))

          (push
           (cons name
                 (xml-substitute-special
                  (substring text
                             value-start
                             value-end)))
           attributes)

          (setq position
                (1+ value-end))))))

    (list element
          (nreverse attributes)
          self-closing
          text)))


(defun my-xml-attribute-editor-read-lines ()
  "Read attributes from the attribute editor buffer."
  (let ((lines
         (split-string
          (buffer-substring-no-properties
           (point-min)
           (point-max))
          "\n"))

        (seen
         (make-hash-table :test 'equal))

        attributes)

    (dolist (line lines)

      (unless
          (string-match-p
           "\\`[[:space:]]*\\'"
           line)

        (unless
            (string-match
             "\\`[ \t]*\\([^[:space:]=<>/]+\\)[ \t]*=[ \t]*\\(.*\\)\\'"
             line)

          (user-error
           "Invalid line: %s\nExpected: name = value"
           line))

        (let ((name
               (match-string 1 line))

              (value
               (match-string 2 line)))

          (when (gethash name seen)
            (user-error
             "Duplicate attribute: %s"
             name))

          (puthash name t seen)

          (push
           (cons name value)
           attributes))))

    (nreverse attributes)))


(defun my-xml-escape-attribute-value (value)
  "Escape VALUE for use inside a double-quoted XML attribute."
  (setq value
        (replace-regexp-in-string
         "&"
         "&amp;"
         value
         t
         t))

  (setq value
        (replace-regexp-in-string
         "\""
         "&quot;"
         value
         t
         t))

  (setq value
        (replace-regexp-in-string
         "<"
         "&lt;"
         value
         t
         t))

  value)

(defun my-xml-attribute-editor-apply ()
  "Apply edited XML attributes to the original buffer."
  (interactive)

  (unless (buffer-live-p my-xml-attribute-editor-source-buffer)
    (user-error "Original XML buffer no longer exists"))

  (let* ((editor-buffer
          (current-buffer))

         (attributes
          (my-xml-attribute-editor-read-lines))

         (source
          my-xml-attribute-editor-source-buffer)

         (start-marker
          my-xml-attribute-editor-source-start)

         (end-marker
          my-xml-attribute-editor-source-end)

         (element
          my-xml-attribute-editor-element-name)

         (self-closing
          my-xml-attribute-editor-self-closing)

         (original-tag
          my-xml-attribute-editor-original-tag)

         (window-configuration
          my-xml-attribute-editor-window-configuration)

         (new-point
          nil)

         (new-tag
          (concat
           "<"
           element

           (when attributes
             (concat
              " "
              (mapconcat
               (lambda (attribute)
                 (format
                  "%s=\"%s\""
                  (car attribute)
                  (my-xml-escape-attribute-value
                   (cdr attribute))))
               attributes
               " ")))

           (if self-closing
               " />"
             ">"))))

    (with-current-buffer source
      (let ((start (marker-position start-marker))
            (end (marker-position end-marker)))

        (unless (and start
                     end
                     (<= start end)
                     (equal (buffer-substring-no-properties start end)
                            original-tag))
          (user-error
           "The XML tag changed while it was being edited; cancel and reopen it"))

        (atomic-change-group
          (delete-region start end)
          (goto-char start)
          (insert new-tag)
          (setq new-point (point)))

        ;; Keep the source buffer marked as changed after applying the edit.
        ;; In particular, do not let the temporary editor's clean state hide
        ;; the change from save-buffer.
        (set-buffer-modified-p t)))

    ;; The editor is a temporary view.  Remove its window before restoring the
    ;; source window configuration; otherwise pop-to-buffer can leave two
    ;; windows displaying SOURCE.
    (set-marker start-marker nil)
    (set-marker end-marker nil)
    (kill-buffer editor-buffer)

    (if window-configuration
        (set-window-configuration window-configuration)
      (pop-to-buffer source))

    (with-current-buffer source
      (goto-char new-point))

    (when-let ((window (get-buffer-window source)))
      (set-window-point window new-point)))))

(defun my-xml-attribute-editor-cancel ()
  "Cancel editing XML attributes."
  (interactive)

  (let ((source
         my-xml-attribute-editor-source-buffer))

    (let ((window-configuration
           my-xml-attribute-editor-window-configuration))

      (set-buffer-modified-p nil)

      (set-marker my-xml-attribute-editor-source-start nil)
      (set-marker my-xml-attribute-editor-source-end nil)
      (kill-buffer
       (current-buffer))

      (when
          (buffer-live-p source)

        (if window-configuration
            (set-window-configuration window-configuration)
          (pop-to-buffer source))))))


(define-derived-mode my-xml-attribute-editor-mode
  text-mode
  "XML-Attributes"
  "Major mode for editing XML attributes."

  (setq-local
   header-line-format
   (format
    " Editing <%s>    C-c C-c: apply    C-c C-k: cancel"
    my-xml-attribute-editor-element-name))

  (setq-local truncate-lines t))


(define-key
 my-xml-attribute-editor-mode-map
 (kbd "C-c C-c")
 'my-xml-attribute-editor-apply)

(define-key
 my-xml-attribute-editor-mode-map
 (kbd "C-c C-k")
 'my-xml-attribute-editor-cancel)


(defun my-xml-edit-attributes ()
  "Open an editor for the attributes of the XML element at point."
  (interactive)

  (unless
      (derived-mode-p 'nxml-mode)

    (user-error
     "This command is intended for nxml-mode"))

  (let ((bounds
         (my-xml-tag-bounds)))

    (unless bounds
      (user-error
       "Could not find an XML element at point"))

    (let* ((source
            (current-buffer))

           (start
            (car bounds))

           (end
            (cdr bounds))

           (parsed
            (my-xml-parse-tag
             start
             end))

           (element
            (nth 0 parsed))

           (attributes
            (nth 1 parsed))

           (self-closing
            (nth 2 parsed))

           (original-tag
            (nth 3 parsed))

           (editor
            (generate-new-buffer
             (format
              "*XML attributes: %s*"
              element))))

      (with-current-buffer editor

        (my-xml-attribute-editor-mode)

        (setq-local
         my-xml-attribute-editor-source-buffer
         source)

        (setq-local
         my-xml-attribute-editor-source-start
         (with-current-buffer source
           (copy-marker start)))

        (setq-local
         my-xml-attribute-editor-source-end
         (with-current-buffer source
           (copy-marker end)))

        (setq-local
         my-xml-attribute-editor-element-name
         element)

        (setq-local
         my-xml-attribute-editor-self-closing
         self-closing)

        (setq-local
           my-xml-attribute-editor-original-tag
           original-tag)

        (setq-local
         my-xml-attribute-editor-window-configuration
         (current-window-configuration))

        (setq-local
         header-line-format
         (format
          " Editing <%s>    C-c C-c: apply    C-c C-k: cancel"
          element))

        (dolist (attribute attributes)
          (insert
           (format
            "%s = %s\n"
            (car attribute)
            (cdr attribute))))

        (goto-char
         (point-min))

        (set-buffer-modified-p nil))

      (pop-to-buffer editor))))


(defun my-xml-mode-keys ()
  "Set custom keybindings for XML mode."
  (local-set-key
   (kbd "C-c '")
   'my-xml-edit-attributes))


(defun my-xml-mode-settings ()
  "Set custom settings for XML mode."
  ;; Do not visually wrap long XML lines.
  (setq-local truncate-lines t))


(define-format-all-formatter xmlstarlet
  (:executable "xmlstarlet")
  (:install)
  (:languages "XML")
  (:features)
  (:format
   (format-all--buffer-easy
    executable
    "fo"
    "--indent-spaces" "2")))

(setq-default format-all-formatters
              '(("XML" xmlstarlet)))

(add-hook 'nxml-mode-hook
          (lambda ()
            (setq truncate-lines t)))


(add-hook 'nxml-mode-hook
          'my-xml-mode-keys)

(add-hook 'nxml-mode-hook
          'my-xml-mode-settings)



(provide 'my-xml)

;;; my-xml.el ends here
