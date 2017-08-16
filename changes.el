;; Support code for updating release notes
;;
;; Jason Blevins <jblevins@xbeta.org>

(defun markdown-mode-guide-changes ()
  "Retrieve and process contents of CHANGES.md."
  (let* ((url "https://raw.githubusercontent.com/jrblevin/markdown-mode/master/CHANGES.md")
         (buf (url-retrieve-synchronously url t t)))
    (when buf
      (with-current-buffer buf
        ;; Convert H1 to H2.
        (while (re-search-forward "^#" nil t 1)
          (replace-match "##" nil nil))
        (goto-char (point-min))
        ;; Convert <kbd> tags to Markdown inline code.
        (while (re-search-forward markdown-regex-kbd nil t 1)
          (replace-match (concat "`" (match-string 2) "`") nil nil))
        ;; Return everything but HTTP headers.
        (goto-char (point-min))
        (search-forward "\n\n")
        (buffer-substring (point) (point-max))))))

(defun markdown-mode-guide-insert-changes ()
  "Insert processed contents of CHANGES.md."
  (interactive)
  (let ((changes (markdown-mode-guide-changes)))
    (insert changes)))
