;Some Emacs Lisp routines for manipulating FB source files


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


(defun fb-search-forward-by-argno (funcname n)
  "Searches forward for the next call of a function/sub matching a regex with at least n args

Places point after the n-th (counted from 1) arg, and mark at the comma before it.
Assumes function syntax is being used iff a ( follows the function name."
  (interactive "sFunction/Sub name? \nnArgument number? ")
  (setq case-fold-search t)
  (while
      (progn 
	(re-search-forward funcname)
	(skip-chars-forward " \t")
	(condition-case nil
	    (progn (fb-mark-arg n
				(cond ((= (char-after) ?\( )   ;function
				       (prog1 (save-excursion (forward-sexp) (1- (point)))
					 (forward-char)))
				      (t    ;sub
					;not good enough: doesn't correctly restrict from inside a one-line IF
				       (save-excursion (re-search-forward "[^_]$")))))
					;if fb-mark-arg runs without errors, stop the loop, otherwise assume not enough args, repeat
		   nil)
	  (error (pop-mark) t)))))
