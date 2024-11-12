(define A (uint-list->bytevector '(#x01 #x23 #x45 #x67) 'little 8))
(define B (uint-list->bytevector '(#x89 #xab #xcd #xef) 'little 8))
(define C (uint-list->bytevector '(#xfe #xbc #xda #x98) 'little 8))
(define D (uint-list->bytevector '(#x76 #x54 #x32 #x10) 'little 8))

(define byteF
  (lambda (X Y Z)
    (logior (logand X Y) (logand (lognot X) Z))))

(define byteG
  (lambda (X Y Z)
    (logior (logand X Z) (logand Y (lognot Z)))))

(define byteH
  (lambda (X Y Z)
    (logxor X Y Z)))

(define byteI
  (lambda (X Y Z)
    (logxor Y (logior X (lognot Z)))))

(define F
  (lambda (X Y Z)
    (let ([x (bytevector->u8-list X)]
	  [y (bytevector->u8-list Y)]
	  [z (bytevector->u8-list Z)])
      (u8-list->bytevector (map byteF X Y Z)))))

(define G
  (lambda (X Y Z)
    (let ([x (bytevector->u8-list X)]
	  [y (bytevector->u8-list Y)]
	  [z (bytevector->u8-list Z)])
      (u8-list->bytevector (map byteG X Y Z)))))

(define H
  (lambda (X Y Z)
    (let ([x (bytevector->u8-list X)]
	  [y (bytevector->u8-list Y)]
	  [z (bytevector->u8-list Z)])
      (u8-list->bytevector (map byteH X Y Z)))))

(define I
  (lambda (X Y Z)
    (let ([x (bytevector->u8-list X)]
	  [y (bytevector->u8-list Y)]
	  [z (bytevector->u8-list Z)])
      (u8-list->bytevector (map byteI X Y Z)))))

(define (padded msg);;we assume msg as a byte-vector and its length is a multiple of 8, so we can edit it byte by byte
  (let* ([msg-bytes (bytevector-length msg)]
					;to calc how many bytes are needed in the padded msg
					;1, msg modular 64
					;2, we need (msg-bytes + (64 - remainder)) bytes in output
					;3, however, if (64 - remainder) <= 8, we have to add another 64
	 [total-padding-bytes (let ([remainder (- 64 (floor-remainder msg 64))])
				(if (<= remainder 8) (+ msg-bytes remainder 64)
				    (+ msg-bytes remainder)))]
	 [output-msg (make-bytevector (+ msg-bytes total-padding-bytes))])
    (bytevector-copy! msg-bytes 0 output-msg 0 msg-bytes)
    (let ([pad-1 (+ msg-bytes 1)]
	  [pad-0 (+ msg-bytes 1 (- total-padding-bytes 9))])
      (bytevector-u8-set! output-msg #b10000000 msg-bytes)
      (bytevector-fill! output-msg #b00000000 pad-1 pad-0)
      (let ([length-to-pad (logand (* msg-bytes 8) (- (expt 2 64) 1))]
	    [64->u8*8 (lambda (num)
			(let ([output (make-bytevector 8)]
			      [full-mask (- (expt 2 64) 1)])
			  (let loop ([n num] [index 0] [mask-length 56])
			    (when (<= index 7)
			      (let ([mask (- (expt 2 mask-length) 1)])
				(bytevector-u8-set! output
					          (logand n (- full-mask mask))
						  index)
				(loop (logand n mask) (+ index 1) (- mask-length 8)))))
			  output))])
	(bytevector-copy! (64->u8*8 length-to-pad) 0 output-msg pad-0 8))))
  output-msg)

(define T
  (let loop ([n 1])
    (if (= n 65) '()
	(cons (truncate (abs (sin n))) (loop (+ n 1))))))

(define 512bits->16words;;take a 64 long bytevector and return 16 words, which is 16 4-long bytevectors as a list
  (lambda (chunk)
    (list (bytevector-slice chunk 0 4)
	  (bytevector-slice chunk 4 8)
	  (bytevector-slice chunk 8 12)
	  (bytevector-slice chunk 12 16))))

(define (process-512bits block);;take a 512 bits block and process it to get the final A, B, C, D
  
