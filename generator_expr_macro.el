;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating custom generator expressions and my own loop. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun r--loop (arglist func)
  "r--loop (i.e. recursive loop) is a custom loop that iterates
over each element in ARGLIST and binds the function to the element
(without eagerly executing it).
It returns a list of cons cells.

Example usage:

   (r--loop '(1 2 3) 'identity) => '((identity 1) (identity 2) (identity 3))"

  (when arglist
    (cons
     `(,func ,(funcall 'car arglist))
     (r--loop (cdr arglist) func))))

(ert-deftest r--loop ()
  (should (equal (rloop '(1 2 3) 'identity) '((identity 1) (identity 2) (identity 3)))))

(ert-deftest r--loop-empty ()
  (should (equal (rloop '() 'identity) '())))


(defmacro gen--expr (iterate var thru list if condi &optional fn)
  "gen--expr iterates over a list, filters it and optionally binds
the remaining elements with a function (see `r--loop').

ITERATE is a custom keyword for this macro (I wanted to experiment with
adding my own keywords with macros ;-P).

VAR is the variable name that the condition will be using.

THRU is another custom keyword.

LIST is the list containing the elements that should be operated on.

IF is yet another custom keyword.

CONDI is a function that takes a the VAR as a parameter.

FN when provided applies the function to the remaining elements (after
the rest were filtered by CONDI).

Example Usage:
  (gen--expr iterate x thru '(1 2 3) if (> x 2)) => '(3)
  (gen--expr iterate x thru '(1 2 3) if (> x 2) identity) => '((identity 3))"
  
  (let
      ((iterate--correct (string-equal (symbol-name iterate) "iterate"))
       (thru--correct   (string-equal (symbol-name thru) "thru")))
    `(if (and ,iterate--correct ,thru--correct)
	 (if (symbol-function ',fn)
	     (r--loop (seq-filter (lambda (,var) ,condi) ,list) ',fn)
	   (seq-filter (lambda (,var) ,condi) ,list))
       (error "invalid syntax!"))))


(ert-deftest gen--expr ()
  "Unit tests for `gen--expr'."
  (should (equal (gen--expr iterate x thru '(1 2 3) if (> x 2)) '(3)))
  (should (equal (gen--expr iterate x thru '(1 2 3) if (> x 2) identity) '((identity 3))))
  (should (equal (gen--expr iterate x thru nil if (> x 2) identity) nil)))


(provide 'gen--expr)
