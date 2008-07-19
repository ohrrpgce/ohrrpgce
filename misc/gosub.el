(defun bug422-test ()
  "Run on a buffer to insert GOTOs which raise warnings on bug422 symptoms when compiled"
  (interactive)
  (widen)
  (save-excursion
    (let ((labelstart 1) (labelcnt 1))
      (goto-char (point-min))
      (while (< (point) (point-max))
        ;consider one sub/function or toplevel at a time
        (narrow-to-region (point) (or (re-search-forward "^end \\(sub\\|function\\)\\|\\(^sub \\|^function \\)" nil t) (point-max)))
        (goto-char (point-min))
        ;place a label and goto for each gosub
        (while (re-search-forward "^[^'\n]*gosub " nil 0)
          (end-of-line)
          (insert (concat "\nlab_" (number-to-string labelcnt) ":"))
          (setq labelcnt (1+ labelcnt)))
        (end-of-line 0)
        (let ((x labelstart)) 
          (while (< x labelcnt)
            (insert (concat "\ngoto lab_" (number-to-string x)))
            (setq x (1+ x))))
        (setq labelstart labelcnt)
        ;find beginning of next block
        (forward-line)
        (widen)
        (re-search-forward "^\\(sub\\|function\\) " nil 0)))))


(defun bug422-test-all-files (directory)
  "Runs bug422-test on all files in a directory"
  (interactive "DDirectory to find .bas files? ")
  (dolist (file (directory-files directory t "\\.bas$"))
    (find-file file)
    (bug422-test)
    (save-buffer)
    (kill-buffer nil)))
