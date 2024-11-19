(define word->num
  (lambda (bv)
    (display "running word->num for bv=")(display bv)(newline)
    (let ([nl (bytevector->u8-list bv)])
      (logior (ash (list-ref nl 0) 24)
	 (ash (list-ref nl 1) 16)
	 (ash (list-ref nl 2) 8)
	 (list-ref nl 3)))))

(define num->word;note that now it returns a list which is not a real word
  (lambda (num)
    (display "running num->word for num=")(display num)(newline)
    (let loop ([num num])
      (if (<= (integer-length num) 8)
	  (list num)
	  (let* ([mask (- (expt 2 8) 1)]
		 [tail (logand mask num)]
		 [head (ash (- num tail) -8)])
	    (append (loop head) (list tail)))))))
