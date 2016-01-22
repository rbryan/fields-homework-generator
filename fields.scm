(use-modules (ice-9 regex) 	;regex
	     (ice-9 popen) 	;maxima process
	     (ice-9 rdelim))	;read-line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Usefull little things

;;Defining a simple eval statement
(define old-eval eval)

(define (eval statement)
  (old-eval statement (current-dynamic-state)))

;;Print a list of items
(define (print . s)
    (set! s (map (lambda (x) (regexp-substitute/global #f "\\$\\$" x 'pre "$" 'post)) s))
    (set! s (map (lambda (x) (regexp-substitute/global #f "(\\\\%)([a-zA-Z][a-zA-Z0-9]*)" x 'pre "\\" 2 'post)) s))
    (set! s (map (lambda (x) (regexp-substitute/global #f "([a-zA-Z%][a-zA-Z0-9]*)(\\\\_)([a-zA-Z%][a-zA-Z0-9]*)" x 'pre 1 "_{" 3 "}" 'post)) s))
    (map display s))


(define (println . s)
  (apply print s)
  (display "\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Stuff for controlling maxima.

;;Start a background maxima session
;;and return the port to communicate with it.
;;A new session is created for each problem.
(define (start-maxima-session)
  (let* ((port (open-pipe* OPEN_BOTH "maxima" "--very-quiet")))
    (display "display2d : false$" port)
    (display "linel: 10000$" port)
    port))

;;Send a string to the running maxima session.
(define (maxima-send s)
  ;(println "Sent: " s)
  (display s maxima-port))

;;Read a line from the maxima session.
(define (maxima-read)
  (read-line maxima-port))

;;Enable or disable simplification in maxima.
(define (set-simplification b)
  (if b
    (maxima-send "simp:true$")
    (maxima-send "simp:false$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Utilities

;;Convert a string to latex using the running
;;maxima session.
(define (string->tex s)
  (let ((s (regexp-substitute/global #f "([a-zA-Z%][a-zA-Z0-9]*)" s 'pre "'" 1 'post)))
    (set-simplification #f)
    (maxima-send (string-append "tex(" s " )$"))
    (let ((result (maxima-read)))
      (set-simplification #t)
      result)))

;;Evaluate a statement in maxima.
;;Simplest use case.
;;Be careful if the output produces more than
;;one line.
(define (maxima-eval s)
  (maxima-send s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Latex functions
(define (hline)
  (println "\\hline"))

(define (vspace)
  (step "" "" ""))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;User functions

;;Solve a problem
(define (prob name steps)
  (set! maxima-port (start-maxima-session))
  (println "\\begin{section}{" name "}")
  (println "\\begin{center}")
  (println "\\begin{tabular}{ m{150pt} | m{150pt} | m{150pt} }")
  (map (lambda (s) (eval s))
       steps)
  (println "\\end{tabular}")
  (println "\\end{center}")
  (println "\\end{section}"))


(define (solve eqn var . exp)
  (let ((exp (if (pair? exp) (car exp) ""))
	(math (string-append "solve(" eqn "," var ")"))
	(words(string-append "Solve " (string->tex eqn) 
				    "for" (string->tex var)".")))
    (println math)
    (println words)
    (maxima-tex-eval math words exp)))
  
(define (show-then-do code words exp)
  (step (string->tex code) words exp)
  (maxima-tex-eval code "" ""))



;;
;;Assignment operators
;;

(define-syntax def-assignment-op
  (syntax-rules ()
		((def-assignment-op name op) 
		   (define (name a b . exp)
			(let ((exp (if (pair? exp) (car exp) "")))
			  (maxima-tex-eval (string-append "'" a "=" b) "" exp)
			  (maxima-send (string-append a op b "$")))))))

(def-assignment-op fun ":=")
(def-assignment-op set ":")




;;
;;Standard infix operators
;;

(define-syntax def-standard-infix
  (syntax-rules (a b)
    ((def-standard-infix name op verb a ideka b)
     (define (name a b . exp)
  	   (let ((exp (if (pair? exp) (car exp) ""))
  		 (math (string-append a op b))
  		 (words (string-append verb
  				(string->tex a)
  				ideka
  				(string->tex b)".")))
  	     (step (string->tex math) words exp)
  	     (maxima-tex-eval math "Simplify" ""))))))

(def-standard-infix mul "*" "Multiply " a " by " b)
(def-standard-infix div "/" "Divide " a " by " b)
(def-standard-infix add "+" "Add " a " to " b)
(def-standard-infix sub "-" "Subtract " a " from " b)
(def-standard-infix dot "." "Dot " a " onto " b)



			

;;Evaluate a statement in maxima as a step.
(define (maxima-simple-eval maxima-code words explanation)
  (maxima-send (string-append maxima-code "$"))
  (let ((result (maxima-read)))
    (step result words explanation))) 

(define (maxima-tex-eval maxima-code words explanation)
  (maxima-simple-eval (string-append "tex(" maxima-code ")") words explanation))

(define (step latex words explanation)
  (print latex " & " words " & " explanation " \\\\\n"))




	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Main

(define maxima-port 0)

(define (main)
	(println "\\documentclass[a4paper,12pt,oneside]{article}")
	(println "\\usepackage{array}")
	(println "\\begin{document}")

	(let loop ((s (read)))
	  (if (not (eof-object? s))
	    (begin
	      (eval s)
	      (loop (read)))))

	(println "\\end{document}"))

(main)
