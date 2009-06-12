(def prif (exp)
  " Print the expression if it is 'true' "
  (if exp (pr exp))) 

(= bodops* (fill-table (table)
   '(mac 2 let 2 with 1 while 1 def 2 fn 1 rfn 2 afn 1
     when 1 unless 1 after 1 whilet 2 for 3 each 2 whenlet 2 awhen 1
     whitepage 0 tag 1 form 1 aform 1 aformh 1 w/link 1 textarea 3 on 2 )))

(= pprsyms* (fill-table (table) 
			'(quote "'" 
			  quasiquote "`"
			  unquote ","
			  unquote-splicing ",@")))

(def sp ((o n 1))
  " Print a number of spaces. "
  (repeat n (pr " ")))

(def print-spaced (xs)
  " Print the expressions in the list separated by spaces. "
  (when xs
    (print car.xs)
    (each x cdr.xs (pr " ") print.x)))

(def print (exp)
  " Print an expression on one line, replacing quote, unquote,
    quasiquote, unquote-splicing, and make-br-fn with their respective symbols. " 
  (do (aif (isa exp 'string)
	    (do (pr #\" exp #\") nil)
	   (or atom.exp dotted.exp)
	    (do pr.exp nil)
	   (pprsyms* car.exp)
	    (do pr.it
	       (print cadr.exp)
	       nil)
	   (is car.exp 'make-br-fn)
	    (do (pr "[") (print-spaced cadr.exp) (pr "]") nil)
	   (do (pr "(") print-spaced.exp (pr ")") nil))
      exp))

(= oneline* 35)

(def len (x (o c 0))
  " Measures the length of a string, vector, table, list or dotted list. "
  (if (isa x 'string) (mz:string-length x)
      (isa x 'vec) (mz:vector-length x)
      (isa x 'table) (mz:hash-table-count x)
      (and atom.x x) (+ c 1)
      acons.x (len cdr.x (+ c 1))
      c))

(mac ppr-sub body
  " A helper macro for use in ppr. "
  `(do (unless noindent sp.col)
       (let whole (tostring print.exp)
	 (if (< len.whole oneline*)              
	     (do pr.whole nil)
	     (do (pr "(")
		 ,@body
		 (pr ")")
		 t)))))

(def ppr (exp (o col 0) (o noindent nil))
  " Pretty print. This function displays arc code with proper
    indenting and representation of some syntax, such as quote,
    quasiquote, unquote, unquote-splicing, and make-br-fn. "
  (aif (or atom.exp dotted.exp (is car.exp 'make-br-fn) (pprsyms* car.exp)) ;if it's an atom, dotted list or make-br-fn, print it.
        (do (unless noindent sp.col)
	    print.exp
	    nil)
       (bodops* car.exp)		;if it's a bodop
        (ppr-sub
	 (if (is car.exp 'with)
	     (do (pr "with (")
		 (on e (pair cadr.exp) 
		     [do 
		       (if (mod index 2) 
			   (prn)) 
		       (map (fn (e) (ppr e (+ col 7))) _)])
		 (pr ")"))
	     (let str (tostring:print-spaced (firstn it exp)) ;get the length of the string of the op + parameters
	       (unless (is it 0) pr.str (sp))		  ;unless there are no arguments, print them and a space.
	       (ppr exp.it (+ col len.str 2) t)))
	 (map [do (prn) (ppr _ (+ col 2))]
	      (nthcdr (+ it 1) exp)))
       (ppr-sub
	(let carstr (tostring:print car.exp)
	  pr.carstr
	  (if cdr.exp
	      (do (sp)
		  (with (broke (ppr cadr.exp (+ col len.carstr 2) t)
			 exps cddr.exp)
		    (if exps (sp))
		    (if (and no.broke
			     (all [or atom._ (and (is car._ 'quote) (atom cadr._))]
				  exps))
			print-spaced.exps
			(when exps
			  (if (and (in car.exp 'aif 'if) (> len.exp 4))
			      (on e exps (prn) (ppr e (+ col 2 len.carstr (mod (+ index 1) 2))))
			      (each e exps (prn) (ppr e (+ col 2 len.carstr)))))))))))))