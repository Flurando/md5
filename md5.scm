(use-modules (rnrs bytevectors) (scheme base))

(define A0 (uint-list->bytevector '(#x01 #x23 #x45 #x67) 'little 8))
(define B0 (uint-list->bytevector '(#x89 #xab #xcd #xef) 'little 8))
(define C0 (uint-list->bytevector '(#xfe #xbc #xda #x98) 'little 8))
(define D0 (uint-list->bytevector '(#x76 #x54 #x32 #x10) 'little 8))

(define F
  (lambda (X Y Z)
    (logior (logand X Y) (logand (lognot X) Z))))

(define G
  (lambda (X Y Z)
    (logior (logand X Z) (logand Y (lognot Z)))))

(define H
  (lambda (X Y Z)
    (logxor X Y Z)))

(define I
  (lambda (X Y Z)
    (logxor Y (logior X (lognot Z)))))

#!
(define F
  (lambda (X Y Z)
    (let ([x (bytevector->u8-list (num->word X))]
	  [y (bytevector->u8-list (num->word Y))]
	  [z (bytevector->u8-list (num->word Z))])
      (u8-list->bytevector (map byteF X Y Z)))))

(define G
  (lambda (X Y Z)
    (let ([x (bytevector->u8-list (num->word X))]
	  [y (bytevector->u8-list (num->word Y))]
	  [z (bytevector->u8-list (num->word Z))])
      (u8-list->bytevector (map byteG X Y Z)))))

(define H
  (lambda (X Y Z)
    (let ([x (bytevector->u8-list (num->word X))]
	  [y (bytevector->u8-list (num->word Y))]
	  [z (bytevector->u8-list (num->word Z))])
      (u8-list->bytevector (map byteH X Y Z)))))

(define I
  (lambda (X Y Z)
    (let ([x (bytevector->u8-list (num->word X))]
	  [y (bytevector->u8-list (num->word Y))]
	  [z (bytevector->u8-list (num->word Z))])
      (u8-list->bytevector (map byteI X Y Z)))))
!#

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

#!
(define T
  (let loop ([n 1])
    (if (= n 65) '()
	(cons (inexact->exact (floor (* 4294967296 (abs (sin n))))) (loop (+ n 1))))))
!#

(define T '(3614090360 3905402710 606105819 3250441966 4118548399 1200080426 2821735955 4249261313 1770035416 2336552879 4294925233 2304563134 1804603682 4254626195 2792965006 1236535329 4129170786 3225465664 643717713 3921069994 3593408605 38016083 3634488961 3889429448 568446438 3275163606 4107603335 1163531501 2850285829 4243563512 1735328473 2368359562 4294588738 2272392833 1839030562 4259657740 2763975236 1272893353 4139469664 3200236656 681279174 3936430074 3572445317 76029189 3654602809 3873151461 530742520 3299628645 4096336452 1126891415 2878612391 4237533241 1700485571 2399980690 4293915773 2240044497 1873313359 4264355552 2734768916 1309151649 4149444226 3174756917 718787259 3951481745));already checked the first number is equal to the rfc's

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
    (display "running word->num for bv=")(display bv)(newline)
    (let ([nl (bytevector->u8-list bv)])
      (logior (ash (list-ref nl 0) 24)
	 (ash (list-ref nl 1) 16)
	 (ash (list-ref nl 2) 8)
	 (list-ref nl 3)))))

(define num->word;now it returns a list which is not a real word
  (lambda (num)
    (display "running num->word for num=")(display num)(newline)
    (let loop ([num num])
      (if (<= (integer-length num) 8)
	  (list num)
	  (let* ([mask (- (expt 2 8) 1)]
		 [tail (logand mask num)]
		 [head (ash (- num tail) -8)])
	    (append (loop head) (list tail)))))))

(define-syntax md5+
  (syntax-rules ()
    [(_ n1 n2 ...)
     (logand (- (expt 2 32) 1) (+ n1 n2 ...))]))
	    
(define md5<<<;this is only intended for n1 which is a 32bit representable int
  (lambda (n1 n2)
    (display "calling md5<<< with ")(display n1)(display " ")(display n2)(newline)
    (logior (ash n1 n2) (ash n1 (- n2 32)))))

;;;a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s).
#!
(define-syntax round-1!
  (syntax-rules ()
    [(_ a b c d k s i)
     (set! a (+ b (md5<<< (md5+ a (F b c d) (bytevector-u8-ref X k) (list-ref T i)) s)))]))
!#

(define-syntax round-1!
  (syntax-rules ()
    [(_ a b c d k s i X)
     (let* ([Xk (begin (display "calculating for kth in X")(newline) (bytevector-u8-ref X k))]
	    [Ti (begin (display "calculating for ith in T")(newline) (list-ref T (- i 1)))]
	    [Fbcd (begin (display "calculating for (F b c d)")(newline) (F b c d))]
	    [md5addsum (begin (display "calculating md5+ ...")(newline) (md5+ a Fbcd Xk Ti))]
	    [shiftedsum (begin (display "calculating md5<<< ...")(newline) (md5<<< md5addsum s))]
	    [lastsum (begin (display "calculating (+ b ...")(newline) (+ b shiftedsum))])
       (set! a lastsum))]))

(define-syntax round-2!
  (syntax-rules ()
    [(_ a b c d k s i X)
     (set! a (+ b (md5<<< (md5+ a (G b c d) (bytevector-u8-ref X k) (list-ref T (- i 1))) s)))]))

(define-syntax round-3!
  (syntax-rules ()
    [(_ a b c d k s i X)
     (set! a (+ b (md5<<< (md5+ a (H b c d) (bytevector-u8-ref X k) (list-ref T (- i 1))) s)))]))

(define-syntax round-4!
  (syntax-rules ()
    [(_ a b c d k s i X)
     (set! a (+ b (md5<<< (md5+ a (I b c d) (bytevector-u8-ref X k) (list-ref T (- i 1))) s)))]))

(define (process-512bits block);;take a 512 bits block as a 64-long bytevector and process it to get the final A, B, C, D
  (let ([X block]
	[A (word->num A0)] [B (word->num B0)] [C (word->num C0)] [D (word->num D0)])
    (display "X: ")(display X)(newline)
    (let ([AA A] [BB B] [CC C] [DD D])
      (display "round 1!")
      ;[ABCD  0  7  1]  [DABC  1 12  2]  [CDAB  2 17  3]  [BCDA  3 22  4]
     ;[ABCD  4  7  5]  [DABC  5 12  6]  [CDAB  6 17  7]  [BCDA  7 22  8]
     ;[ABCD  8  7  9]  [DABC  9 12 10]  [CDAB 10 17 11]  [BCDA 11 22 12]
     ;[ABCD 12  7 13]  [DABC 13 12 14]  [CDAB 14 17 15]  [BCDA 15 22 16]
      (round-1! A B C D 0 7 1 X)
      (display "round 1 first calc passed!")(newline)
      (round-1! D A B C 1 12 2 X)
      (round-1! C D A B 2 17 3 X)
      (round-1! B C D A 3 22 4 X)
      
      (round-1! A B C D 4 7 5 X)
      (round-1! D A B C 5 12 6 X)
      (round-1! C D A B 6 17 7 X)
      (round-1! B C D A 7 22 8 X)
      
      (round-1! A B C D 8 7 9 X)
      (round-1! D A B C 9 12 10 X)
      (round-1! C D A B 10 17 11 X)
      (round-1! B C D A 11 22 12 X)
      
      (round-1! A B C D 12 7 13 X)
      (round-1! D A B C 13 12 14 X)
      (round-1! C D A B 14 17 15 X)
      (round-1! B C D A 15 22 16 X)

      (display "round 2!")
      ; [ABCD  1  5 17]  [DABC  6  9 18]  [CDAB 11 14 19]  [BCDA  0 20 20]
      ;[ABCD  5  5 21]  [DABC 10  9 22]  [CDAB 15 14 23]  [BCDA  4 20 24]
      ;[ABCD  9  5 25]  [DABC 14  9 26]  [CDAB  3 14 27]  [BCDA  8 20 28]
      ;[ABCD 13  5 29]  [DABC  2  9 30]  [CDAB  7 14 31]  [BCDA 12 20 32]
      (round-2! A B C D 1 5 17 X)
      (round-2! D A B C 6 9 18 X)
      (round-2! C D A B 11 14 19 X)
      (round-2! B C D A 0 20 20 X)
      
      (round-2! A B C D 5 5 21 X)
      (round-2! D A B C 10 9 22 X)
      (round-2! C D A B 15 14 23 X)
      (round-2! B C D A 4 20 24 X)
      
      (round-2! A B C D 9 5 25 X)
      (round-2! D A B C 14 9 26 X)
      (round-2! C D A B 3 14 27 X)
      (round-2! B C D A 8 20 28 X)
      
      (round-2! A B C D 13 5 29 X)
      (round-2! D A B C 2 9 30 X)
      (round-2! C D A B 7 14 31 X)
      (round-2! B C D A 12 20 32 X)

      (display "round 3!")
      ;[ABCD  5  4 33]  [DABC  8 11 34]  [CDAB 11 16 35]  [BCDA 14 23 36]
     ;[ABCD  1  4 37]  [DABC  4 11 38]  [CDAB  7 16 39]  [BCDA 10 23 40]
     ;[ABCD 13  4 41]  [DABC  0 11 42]  [CDAB  3 16 43]  [BCDA  6 23 44]
     ;[ABCD  9  4 45]  [DABC 12 11 46]  [CDAB 15 16 47]  [BCDA  2 23 48]
      (round-3! A B C D 5 4 33 X)
      (round-3! D A B C 8 11 34 X)
      (round-3! C D A B 11 16 35 X)
      (round-3! B C D A 14 23 36 X)

      (round-3! A B C D 1 4 37 X)
      (round-3! D A B C 4 11 38 X)
      (round-3! C D A B 7 16 39 X)
      (round-3! B C D A 10 23 40 X)

      (round-3! A B C D 13 4 41 X)
      (round-3! D A B C 0 11 42 X)
      (round-3! C D A B 3 16 43 X)
      (round-3! B C D A 6 23 44 X)

      (round-3! A B C D 9 4 45 X)
      (round-3! D A B C 12 11 46 X)
      (round-3! C D A B 15 16 47 X)
      (round-3! B C D A 2 23 48 X)

      (display "round 4!")
      ;[ABCD  0  6 49]  [DABC  7 10 50]  [CDAB 14 15 51]  [BCDA  5 21 52]
     ;[ABCD 12  6 53]  [DABC  3 10 54]  [CDAB 10 15 55]  [BCDA  1 21 56]
     ;[ABCD  8  6 57]  [DABC 15 10 58]  [CDAB  6 15 59]  [BCDA 13 21 60]
     ;[ABCD  4  6 61]  [DABC 11 10 62]  [CDAB  2 15 63]  [BCDA  9 21 64]
      (round-4! A B C D 0 6 49 X)
      (round-4! D A B C 7 10 50 X)
      (round-4! C D A B 14 15 51 X)
      (round-4! B C D A 5 21 52 X)

      (round-4! A B C D 12 6 53 X)
      (round-4! D A B C 3 10 54 X)
      (round-4! C D A B 10 15 55 X)
      (round-4! B C D A 1 21 56 X)

      (round-4! A B C D 8 6 57 X)
      (round-4! D A B C 15 10 58 X)
      (round-4! C D A B 6 15 59 X)
      (round-4! B C D A 13 21 60 X)

      (round-4! A B C D 4 6 61 X)
      (round-4! D A B C 11 10 62 X)
      (round-4! C D A B 2 15 63 X)
      (round-4! B C D A 9 21 64 X)

      (display "the four round ended!")(newline)
      (set! A0 (num->word (md5+ A AA)))
      (set! B0 (num->word (md5+ B BB)))
      (set! C0 (num->word (md5+ C CC)))
      (set! D0 (num->word (md5+ D DD))))))

(define padded-msg->512bits
  (lambda (padded-msg)
    (display "running padded-msg->512bits")(newline)
    (display "with value passed in as: ")(display padded-msg)(newline)
    (let* ([total (/ (bytevector-length padded-msg) 64)]
	   [ans '()])
      (display "total: ")(display total)(newline)
      (display "ans: ")(display ans)(newline)
      (let loop ([n 0])
	(display "n: ")(display n)(newline)
	(unless (= n (bytevector-length padded-msg))
	  (set! ans (cons (bytevector-copy padded-msg n (+ n 64)) ans))
	  (display "ans: ")(display ans)(newline)
	  (loop (+ n 64))))
      (display "ans: ")(display ans)(newline)
      ans)))

(define do-md5
  (lambda (raw-msg);expecting a bytevector
    (display "do-md5 called!")(newline)
    (let* ([padded-msg (padded raw-msg)]
	   [length-in-blocks (/ (bytevector-length padded-msg) 64)]
	   [512bits-blocks (padded-msg->512bits padded-msg)])
      (let loop ([n 0])
	(unless (>= n length-in-blocks)
	  (process-512bits (list-ref 512bits-blocks n))
	  (loop (+ n 1)))))))

(define (yell-md5)
  (let* ([total (append A0 B0 C0 D0)]
	 [total-length (length total)])
    (let loop ([n 0])
      (unless (= n total-length)
	(begin
	  (format #t "~x" (list-ref total n))
	  (loop (+ n 1)))))))

(define main
  (lambda ()
    (let ([test-ans (do-md5 (string->utf8 ""))])
      (display "yelling!")
      (newline)
      (yell-md5))));the calc passed but the answer is wrong:(
