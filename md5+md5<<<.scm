(define-syntax md5+
  (syntax-rules ()
    [(_ n1 n2 ...)
     (begin (display "md5+ procedure: integer-length: ")(display (integer-length (+ n1 n2 ...)))(newline) (floor-remainder (+ n1 n2 ...) (expt 2 32)))]));maybe the problem is from here!
	    
(define md5<<<;this is only intended for n1 which is a 32bit representable int
  (lambda (n1 n2)
    (display "calling md5<<< with (n1 n2): ")(display n1)(display " ")(display n2)(newline)
    (logior (ash n1 n2) (ash n1 (- n2 32)))))
