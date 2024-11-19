(define (padded msg);tested for #vu8() and #vu8(1 2 3)
  (let* ([msg (bytevector-copy msg)]
	 [msg-length (bytevector-length msg)]
	 [remainder (if (zero? msg-length)
			0
			(floor-remainder msg-length 64))]
	 [total-padding-bytes (if (>= remainder 56) (- 128 remainder)
				  (- 64 remainder))]
	 [total-length (+ msg-length total-padding-bytes)]
	 [output-msg (make-bytevector total-length 0)])
    (display "padded is called!")(newline)
    (bytevector-copy! msg 0 output-msg 0 msg-length)
    (bytevector-u8-set! output-msg msg-length 128)
    (let ([msg-length-binary (integer-length msg-length)])
      (bytevector-u8-set! output-msg (- total-length 8) (logand (ash msg-length-binary 56) #xFF))
      (bytevector-u8-set! output-msg (- total-length 7) (logand (ash msg-length-binary 48) #xFF))
      (bytevector-u8-set! output-msg (- total-length 6) (logand (ash msg-length-binary 40) #xFF))
      (bytevector-u8-set! output-msg (- total-length 5) (logand (ash msg-length-binary 32) #xFF))
      (bytevector-u8-set! output-msg (- total-length 4) (logand (ash msg-length-binary 24) #xFF))
      (bytevector-u8-set! output-msg (- total-length 3) (logand (ash msg-length-binary 16) #xFF))
      (bytevector-u8-set! output-msg (- total-length 2) (logand (ash msg-length-binary 8) #xFF))
      (bytevector-u8-set! output-msg (- total-length 1) (logand msg-length-binary #xFF)))
    (display "padding step is over! returning...")(newline)
    output-msg))
