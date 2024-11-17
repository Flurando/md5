(use-modules (rnrs bytevectors) (scheme base))

(define A0 (uint-list->bytevector '(#x01 #x23 #x45 #x67) 'little 8))
(define B0 (uint-list->bytevector '(#x89 #xab #xcd #xef) 'little 8))
(define C0 (uint-list->bytevector '(#xfe #xbc #xda #x98) 'little 8))
(define D0 (uint-list->bytevector '(#x76 #x54 #x32 #x10) 'little 8))

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

;;;this function despirately need rewriting because using int to calculate stuff is can lead to stackoverflow even for the shortest padding(64 2s). Try to implement this pad function and the later logic of the main rotation calculation to use bytevectors instead.
(define (padded msg);;we assume msg as a byte-vector and its length is a multiple of 8, so we can edit it byte by byte
  (let* ([msg-bytes (bytevector-length msg)]
					;to calc how many bytes are needed in the padded msg
					;1, msg modular 64
					;2, we need (msg-bytes + (64 - remainder)) bytes in output
					;3, however, if (64 - remainder) <= 8, we have to add another 64
	 [total-padding-bytes (let ([remainder (- 64 (floor-remainder msg-bytes 64))])
				(display "remainder: ")(display remainder)
				(newline)
				(if (<= remainder 8) (begin (display "if is true!")(newline) (+ remainder 64))
				    (begin (display "if is false!")(newline)
					   remainder)))]
	 [full-length (+ msg-bytes total-padding-bytes)]
	 [output-msg (make-bytevector full-length)])
    (display "output-msg: ")(display output-msg)(newline)
    (display "msg-bytes: ")(display msg-bytes)
    (newline)
    (display "total-padding-bits: ")(display total-padding-bytes)
    (newline)
    (display "output-msg length: ")(display (+ msg-bytes total-padding-bytes))
    (newline)
    (bytevector-copy! msg 0 output-msg 0 msg-bytes)
    (let ([pad-1 msg-bytes]
	  [pad-0 (+ msg-bytes (- total-padding-bytes 8))])
      (display "now we are feeding ")(display msg-bytes)(display " as the index")
      (newline)
      (display "output-msg: ")(display output-msg)(newline)
      (bytevector-u8-set! output-msg msg-bytes #x80)
      (display "output-msg: ")(display output-msg)(newline)
      (bytevector-fill! output-msg 0 (+ 1 pad-1) pad-0)
      (display "output-msg: ")(display output-msg)(newline)
      (let* ([length-to-pad (logand (* msg-bytes 8) (- (expt 2 64) 1))]
	     [bv ((lambda (num)
		    (display "the program flag [1]")
		    (let ([output (make-bytevector 8)]
			  [full-mask (- (expt 2 64) 1)])
		      (display "output: ")(display output)(newline)
		      (display "full-mask")(display full-mask)(newline)
		      (let loop ([n num] [index 0] [mask-length 56])
			(if (<= index 7)
			    (begin
			      (display "this round, n: ")(display n)(newline)
			      (display "index: ")(display index)(newline)
			      (display "mask-length: ")(display mask-length)(newline)
			      (let ([mask (- (expt 2 mask-length) 1)])
				(display "output: ")(display output)(newline)
				(display "(- full-mask mask): ")(display (- full-mask mask))(newline)
				(display "future logand n (- full-mask mask): ")(display (logand n (- full-mask mask)))(newline)
				(bytevector-u8-set! output
						    index
					            (ash (logand n (- full-mask mask)) (- mask-length)))
				(display "finished round")(newline)
				(unless (< mask-length 0) (begin (display "loop!")(newline) (loop (logand n mask) (+ index 1) (- mask-length 8))))))
			    (begin (display "index is >7 now! out of looping")(newline))))
		      (display "going to return output soon")(newline)
		      output)) length-to-pad)])
	(display "going on...")(newline)
	(display "bv: ")(display bv)(newline)
	(display "(- full-length 8): ")(display (- full-length 8))(newline)
	(bytevector-copy! bv 0 output-msg (- full-length 8) 8)
	(display "copy passed!")(newline)
	(display "output-msg: ")(display output-msg)(newline)))
  output-msg))

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

(define make-M;;This produces the list of words in padded msg which is a bytevector
  (lambda (padded-msg)
    (let* ([msg-length (bytevector-length padded-msg)]
	   [number-of-512bits (/ msg-length 512)])
      (let loop ([n number-of-512bits] [c 0])
	(if (= n 0) '()
	    (append (512bits->16words (bytevector-slice padded-msg c (+ 16 c))) (loop (- n 1) (+ 16 c))))))))

(define word->num
  (lambda (bv)
    (let ([nl (bytevector->u8-list bv)])
      (+ (* (list-ref nl 0) (expt 2 23))
	 (* (list-ref nl 1) (expt 2 15))
	 (* (list-ref nl 2) (expt 2 7))
	 (list-ref nl 3)))))

(define num->word
  (lambda (num)
    (let loop ([num num])
      (if (= (integer-length num) 8)
	  (list num)
	  (let* ([mask (- (expt 2 8) 1)]
		 [tail (logand mask num)]
		 [head (ash (- num tail) 8)])
	    (append (loop head) (list tail)))))))

(define-syntax md5+
  (syntax-rules ()
    [(_ n1 n2 ...)
     (floor-remainder (+ n1 n2 ...) (expt 2 32))]))

(define md5<<<
  (lambda (n1 n2)
    (let* ([mask (ash (- (expt 2 n2) 1) (- 32 n2))]
	   [head (logand n1 mask)]
	   [n (ash (- n1 head) n2)])
      (+ n head))))

;;;a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s).
(define-syntax round-1!
  (syntax-rules ()
    [(_ a b c d k s i)
     (set! a (+ b (md5<<< (md5+ a (F b c d) (bytevector-ref X k) (list-ref T i)) s)))]))

(define-syntax round-2!
  (syntax-rules ()
    [(_ a b c d k s i)
     (set! a (+ b (md5<<< (md5+ a (G b c d) (bytevector-ref X k) (list-ref T i)) s)))]))

(define-syntax round-3!
  (syntax-rules ()
    [(_ a b c d k s i)
     (set! a (+ b (md5<<< (md5+ a (H b c d) (bytevector-ref X k) (list-ref T i)) s)))]))

(define-syntax round-4!
  (syntax-rules ()
    [(_ a b c d k s i)
     (set! a (+ b (md5<<< (md5+ a (I b c d) (bytevector-ref X k) (list-ref T i)) s)))]))

(define (process-512bits block);;take a 512 bits block as a 64-long bytevector and process it to get the final A, B, C, D
  (let ([X block]
	[A (word->num A0)] [B (word->num B0)] [C (word->num C0)] [D (word->num D0)])
    (let ([AA A] [BB B] [CC C] [DD D])
      ;[ABCD  0  7  1]  [DABC  1 12  2]  [CDAB  2 17  3]  [BCDA  3 22  4]
     ;[ABCD  4  7  5]  [DABC  5 12  6]  [CDAB  6 17  7]  [BCDA  7 22  8]
     ;[ABCD  8  7  9]  [DABC  9 12 10]  [CDAB 10 17 11]  [BCDA 11 22 12]
     ;[ABCD 12  7 13]  [DABC 13 12 14]  [CDAB 14 17 15]  [BCDA 15 22 16]
      (round-1! A B C D 0 7 1)
      (round-1! D A B C 1 12 2)
      (round-1! C D A B 2 17 3)
      (round-1! B C D A 3 22 4)
      
      (round-1! A B C D 4 7 5)
      (round-1! D A B C 5 12 6)
      (round-1! C D A B 6 17 7)
      (round-1! B C D A 7 22 8)
      
      (round-1! A B C D 8 7 9)
      (round-1! D A B C 9 12 10)
      (round-1! C D A B 10 17 11)
      (round-1! B C D A 11 22 12)
      
      (round-1! A B C D 12 7 13)
      (round-1! D A B C 13 12 14)
      (round-1! C D A B 14 17 15)
      (round-1! B C D A 15 22 16)

      ; [ABCD  1  5 17]  [DABC  6  9 18]  [CDAB 11 14 19]  [BCDA  0 20 20]
      ;[ABCD  5  5 21]  [DABC 10  9 22]  [CDAB 15 14 23]  [BCDA  4 20 24]
      ;[ABCD  9  5 25]  [DABC 14  9 26]  [CDAB  3 14 27]  [BCDA  8 20 28]
      ;[ABCD 13  5 29]  [DABC  2  9 30]  [CDAB  7 14 31]  [BCDA 12 20 32]
      (round-2! A B C D 1 5 17)
      (round-2! D A B C 6 9 18)
      (round-2! C D A B 11 14 19)
      (round-2! B C D A 0 20 20)
      
      (round-2! A B C D 5 5 21)
      (round-2! D A B C 10 9 22)
      (round-2! C D A B 15 14 23)
      (round-2! B C D A 4 20 24)
      
      (round-2! A B C D 9 5 25)
      (round-2! D A B C 14 9 26)
      (round-2! C D A B 3 14 27)
      (round-2! B C D A 8 20 28)
      
      (round-2! A B C D 13 5 29)
      (round-2! D A B C 2 9 30)
      (round-2! C D A B 7 14 31)
      (round-2! B C D A 12 20 32)

      ;[ABCD  5  4 33]  [DABC  8 11 34]  [CDAB 11 16 35]  [BCDA 14 23 36]
     ;[ABCD  1  4 37]  [DABC  4 11 38]  [CDAB  7 16 39]  [BCDA 10 23 40]
     ;[ABCD 13  4 41]  [DABC  0 11 42]  [CDAB  3 16 43]  [BCDA  6 23 44]
     ;[ABCD  9  4 45]  [DABC 12 11 46]  [CDAB 15 16 47]  [BCDA  2 23 48]
      (round-3! A B C D 5 4 33)
      (round-3! D A B C 8 11 34)
      (round-3! C D A B 11 16 35)
      (round-3! B C D A 14 23 36)

      (round-3! A B C D 1 4 37)
      (round-3! D A B C 4 11 38)
      (round-3! C D A B 7 16 39)
      (round-3! B C D A 10 23 40)

      (round-3! A B C D 13 4 41)
      (round-3! D A B C 0 11 42)
      (round-3! C D A B 3 16 43)
      (round-3! B C D A 6 23 44)

      (round-3! A B C D 9 4 45)
      (round-3! D A B C 12 11 46)
      (round-3! C D A B 15 16 47)
      (round-3! B C D A 2 23 48)

      ;[ABCD  0  6 49]  [DABC  7 10 50]  [CDAB 14 15 51]  [BCDA  5 21 52]
     ;[ABCD 12  6 53]  [DABC  3 10 54]  [CDAB 10 15 55]  [BCDA  1 21 56]
     ;[ABCD  8  6 57]  [DABC 15 10 58]  [CDAB  6 15 59]  [BCDA 13 21 60]
     ;[ABCD  4  6 61]  [DABC 11 10 62]  [CDAB  2 15 63]  [BCDA  9 21 64]
      (round-4! A B C D 0 6 49)
      (round-4! D A B C 7 10 50)
      (round-4! C D A B 14 15 51)
      (round-4! B C D A 5 21 52)

      (round-4! A B C D 12 6 53)
      (round-4! D A B C 3 10 54)
      (round-4! C D A B 10 15 55)
      (round-4! B C D A 1 21 56)

      (round-4! A B C D 8 6 57)
      (round-4! D A B C 15 10 58)
      (round-4! C D A B 6 15 59)
      (round-4! B C D A 13 21 60)

      (round-4! A B C D 4 6 61)
      (round-4! D A B C 11 10 62)
      (round-4! C D A B 2 15 63)
      (round-4! B C D A 9 21 64)

      (set! A0 (num->word (md5+ A AA)))
      (set! B0 (num->word (md5+ B BB)))
      (set! C0 (num->word (md5+ C CC)))
      (set! D0 (num->word (md5+ D DD))))))

(define padded-msg->512bits
  (lambda (padded-msg)
    (let* ([total (/ (bytevector-length padded-msg) 64)]
	   [ans (make-list total)])
      (let loop ([n 0] [index 0])
	(unless (= n (- total 1))
	  (list-set! ans index (bytevector-copy padded-msg n (+ n 64)))
	  (loop (+ n 64) (+ index 1)))))
    ans))

(define do-md5
  (lambda (raw-msg);expecting a bytevector
    (let* ([padded-msg (padded raw-msg)]
	   [length-in-blocks (/ (bytevector-length padded-msg) 64)]
	   [512bits-blocks (padded-msg->512bits padded-msg)])
      (let loop ([n 0])
	(unless (>= n length-in-blocks)
	  (process-512bits (list-ref 512bits-blocks n))
	  (loop (+ n 1)))))))

(define (yell-md5)
  (let ([A (bytevector->u8-list A0)]
	[B (bytevector->u8-list B0)]
	[C (bytevector->u8-list C0)]
	[D (bytevector->u8-list D0)])
    (let* ([total (append A B C D)]
	   [total-length (length total)])
      (let loop ([n 0])
	(if (= n total-length) #f
	    (begin
	      (format #t "~x" (list-ref total n))
	      (loop (+ n 1))))))))

(define main
  (lambda ()
    (let ([test-ans (do-md5 (string->utf8 "a"))])
      (yell-md5))))
