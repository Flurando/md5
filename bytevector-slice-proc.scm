(define bytevector-slice;already checked situations without guards
  (lambda (chunk index1 index2)
    ;(display "1")(newline)
    (let* ([n (- index2 index1)]
	   [output (make-bytevector n)])
      ;(display "2")(newline)
      (let loop ([index index1] [shadow-index 0])
	;(display "index: ")(display index)(newline)
	(unless (= index index2)
	  (bytevector-u8-set! output shadow-index (bytevector-u8-ref chunk index))
	  (loop (+ index 1) (+ shadow-index 1))))
      output)))
