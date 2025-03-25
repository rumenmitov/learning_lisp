;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execute SQL as code, not strings! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro sql--do (&key db &key op &rest body)
  "Executes the BODY as an sql string in the sqlite database DB.
DB should be an sqlite database object (see `sqlite-open').

Since ELISP has a separate sqlite function for modifying (e.g. inserting, updating, deleting, etc)
and a separate one for retrieving data (e.g. selecting) the sql--do macro uses OP to decide
which type of operation the caller wants to perform.

OP can be either set or get. The set option returns t on success and nil on failure.
The get option returns the result of `sqlite-select'.

One caveat is that comma and semicolon characters MUST be escaped
due to LISP's parsing! Moreover, ALWAYS use double quotes for strings!

Example usage:
  (sql--do :db (sqlite-open) :op get select * from users\\;)
"
  
  (if (not (sqlite-available-p))
      (error "sqlite not available!"))
  (let* ((_query_commas (mapconcat 'prin1-to-string body " "))
	 (_query_semicolons (string-replace "\\," "," _query_commas))
	 (query (string-replace "\\;" ";" _query_semicolons)))
    (cond
     ((eq op 'get) `(sqlite-select ,db ,query))
     ((eq op 'set) `(sqlite-execute-batch ,db ,query))
     (t `(error (concat "invalid operation: " (symbol-name ,op)))))))



(ert-deftest sql--do ()
  "Unit test for `sql--do'."
  (let ((db (sqlite-open)))
    (should (equal
	     (sql--do :db db :op set CREATE TABLE students(name VARCHAR\, age INT\, course VARCHAR)\;
		      INSERT INTO students(name\, age\, course) VALUES("rumen"\, 20\, "cs")\;
		      INSERT INTO students(name\, age\, course) VALUES("kendrick"\, 35\, "music")\;
		      INSERT INTO students(name\, age\, course) VALUES("rihanna"\, 42\, "music")\;)
	     t))
    (should (equal
	     (sql--do :db db :op get SELECT * FROM students\;)
	     '(("rumen" 20 "cs") ("kendrick" 35 "music") ("rihanna" 42 "music"))))
    (sqlite-close db)))


(provide 'sql--do)
