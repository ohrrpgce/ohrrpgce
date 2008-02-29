;type "alt-x load-file <RET> with.el" to load
;and then "alt-x with-gosub-warn-all-files" (or "alt-x with-gosub-warn" with a file open)

(defun add-line-copy-indent ()
  "Add a new line after this one, beginning with exactly the same whitespace."
  (interactive)
  (beginning-of-line)
  (copy-region-as-kill (point) (progn (skip-chars-forward " \t") (point)))
  (end-of-line)
  (newline)
  (yank)
  (pop-mark))

(defun with-gosub-warn ()
  "Insert a warning after every GOSUB out of a WITH block."
  (interactive)
  (widen)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^[[:blank:]]*with" nil 0)
    (while (< (point) (point-max))
      ;have to deal with nested WITH blocks
      (let ((depth 1) (start (point)))
	(while (> depth 0)
	  (re-search-forward "^[[:blank:]]*\\(end \\)?with" nil 0)
	  (setq depth (+ depth (if (match-string 1) -1 1))))
	(narrow-to-region start (point)))
      ;inside a WITH block
      (goto-char (point-min))
      (while (re-search-forward "gosub " nil 0)
	;(forward-line)
	;(indent-relative-maybe)
	(add-line-copy-indent)
	(insert "'--WARNING: WITH pointer probably corrupted"))
      ;find beginning of next block
      (forward-line)
      (widen)
      (re-search-forward "^[[:blank:]]*with" nil 0))))

(defun with-gosub-warn-all-files (directory)
  "Runs with-gosub-warn on all files in a directory"
  (interactive "DDirectory to find .bas files? ")
  (dolist (file (directory-files directory t "\\.bas$"))
    (find-file file)
    (with-gosub-warn)
    (save-buffer)
    (kill-buffer nil)))
