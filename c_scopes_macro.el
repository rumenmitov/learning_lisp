;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding C-like scopes to LISP! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro { (&rest body)
  "C-scopes begin with {, so that is what the macro is called.

It executes all of BODY and checks for a closing }.

More than one sexps are allowed in the scope. As with functions,
only the result of the last sexp should be returned.

Example usage:
  ({ (+ 1 1) }) => 2
  ({ (+ 1 1) (* 2 3) }) => 6
"
  (let
      ((is--closed (eq (car (last body)) '})))
    (if (not is--closed)
	(error "invalid syntax: missing } at the end of the scope!"))
    `(when ,is--closed
       ,@(butlast body))))


(ert-deftest scope()
  "Unit test for `{'."
  (should (eq ({ (+ 1 1) }) 2))
  (should (eq ({ (+ 1 1) (* 2 3) }) 6)))


