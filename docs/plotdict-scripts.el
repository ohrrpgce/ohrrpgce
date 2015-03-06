;;; Emacs scripts for editing plotdict.xml


(defun plotdict-block (title)
  "Given a marked part of the buffer containing some <command> blocks,
puts <section></section> tags around them."
  (interactive "sTitle name: ")
  (save-excursion
    (if (< (mark) (point))
        (exchange-point-and-mark))
    (beginning-of-line)
    (insert "		<section title=\"" title "\">\n")
    (exchange-point-and-mark)
    (beginning-of-line)
    (insert "		</section>		<!-- " title "-->\n")))
