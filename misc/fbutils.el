;Some Emacs Lisp routines for manipulating FB source files
;because visual-basic-mode is unusable for FB

(defmacro override-optional (override bindings &rest body)
  "If override, for each pair (sym1 sym2) in bindings temporarily (in body) set the \
function cell of sym1 to sym2, as in (setf 'sym1 'sym2)"
  (if override 
      (cons 'letf
	    (cons
	     (mapcar (lambda (elem)
		       `((symbol-function ',(car elem)) ',(cadr elem)))
		     bindings)
	     body))
    (cons 'progn body)))


(defun add-line-copy-indent ()
  "Add a new line after this one, beginning with exactly the same whitespace."
  (interactive)
  (beginning-of-line)
  (copy-region-as-kill (point) (progn (skip-chars-forward " \t") (point)))
  (end-of-line)
  (newline)
  (yank)
  (pop-mark))


(defun fb-narrow-to-arglist ()
  "Assuming point is after a function or sub name, narrow buffer to its arglist,
not including parentheses if any."
  (interactive)
  (skip-chars-forward " \t")
  (cond ((= (char-after) ?\( )	;function
	 (narrow-to-region (1+ (point))
			   (save-excursion (forward-sexp) (1- (point)))))
	(t			;sub
					;not good enough: doesn't correctly restrict from inside a one-line IF
	 (narrow-to-region (point)
			   (save-excursion (re-search-forward "[^_]$"))))))


;TODO: turn this into a func to move over expressions
(defun fb-mark-arg (args &optional limit)
  "In the middle of an argument list, set the region around the args-th arg on from point."
  (interactive "nSkip forward how many args? ")
  (push-mark)
  (when (char-equal (char-after) ?, ) (forward-char))
  (let (char (argno 1))
    (catch 'done
      (while		;when search fails, go to limit and break if on last arg
	  (re-search-forward "[,(\"]" limit (if (eq argno args) 0 nil))
;	(backward-char)
	(setq char (char-before))
	(cond ((char-equal char ?\( ) (backward-char) (forward-sexp 1))
;	      ((char-equal char ?\) ) (error "not enough arguments"))
	      ((char-equal char ?, ) (if (<= (incf argno) args) (set-mark (1- (point)))
				       (throw 'done nil)))
					;I assume forward-sexp uses normal escape codes, not ideal
	      ((char-equal char ?\") (backward-char) (forward-sexp 1))))))
;      (forward-char))))
  (when (< (point) (or limit (buffer-end 1))) (backward-char)))
;  (when (> args 0) (backward-char)))


(defun fb-enum-args ()
  "Assuming point is after a function or sub name, which has n arguments,
return a list length n+1 of the positions of each argument plus the end of arglist."
  (save-excursion
    (save-restriction
      (fb-narrow-to-arglist)
      (let (char
	    (argno 1)
	    (ret (list (point))))
	(while		;when search fails, go to limit and break if on last arg
	    (re-search-forward "[,(\"]" nil 0)
	  (setq char (char-before))
	  (cond ((char-equal char ?\( ) (backward-char) (forward-sexp 1))
	      ((char-equal char ?, ) (setq ret (append ret (list (1- (point))))))
					;I assume forward-sexp uses normal escape codes, not ideal
	      ((char-equal char ?\") (backward-char) (forward-sexp 1))))
	(when (< (point) (buffer-end 1)) (backward-char))
	(append ret (list (point)))))))


(defun fb-search-forward-by-argno (funcname n)
  "Searches forward for the next call of a function/sub matching a regex with at least abs(n) args.

Specify the argument number either with positive n starting from 1,
or negative n starting from -1 to count from the end of the arglist backwards.
Places point after the argument, and mark before it.
Assumes function syntax is being used iff a ( follows the function name."

  (interactive "sFunction/Sub name? \nnArgument number? ")
  (setq case-fold-search t)
  (let (argloc argnum argno)
    (while
	(progn 
	  (re-search-forward (concat funcname "[a-z_]*"))
	  (skip-chars-forward " \t")
	  (setq argloc (fb-enum-args))
	  (setq argnum (1- (length argloc)))
	  (setq argno (if (< n 0)
			  (+ n argnum 1)
			n))
					;skip if this call does not have enough arguments
	  (or (<= argno 0) (> argno argnum))))
    (push-mark (elt argloc (1- argno)))
    (goto-char (elt argloc argno))))

(defun replace-open ()
  "Convert OPEN to OPENFILE. Does not support access read write, lock, encoding, len.
Move point to start of OPEN (or before it on the same line) before invoking.
Queries for every following OPEN in the file as well.

E.g.
  OPEN fi FOR BINARY ACCESS READ AS #fh ''asd
To
  OPENFILE(fi, FOR_BINARY + ACCESS_READ, fh) ''asd
"
  (interactive)
  (let (add-bracket
        (line-end (save-excursion
                    (end-of-line)
                    (point-marker))))
    (setq case-fold-search t)
    (search-forward "open" line-end)
    (replace-match "openfile")
    (unless (looking-at "(")
      ;; delete space
      (delete-char 1)
      (insert "(")
      (setq add-bracket t))
    (search-forward " for " line-end)
    (delete-char -1)
    (insert "_")
    (backward-word)
    (delete-char -1)
    (insert ", ")
    (when (search-forward " access " line-end t)
      (replace-match " + access_"))
    (search-forward " as " line-end)
    (replace-match ", ")
    (when (search-forward "#" line-end t)
      (delete-char -1))
    (forward-word)
    (when add-bracket
      (insert ")"))
    (when (equal (read-event "Press ENTER to continue") 'return)
      (re-search-forward "open.* for .*as")
      (goto-char (match-beginning 0))
      (when (equal (read-event "Press ENTER to repeat") 'return)
        (undo-boundary)
        (replace-open)))))
