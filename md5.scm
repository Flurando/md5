(use-modules (rnrs bytevectors) (scheme base))

(load "four-initial-words.scm")

(load "four-actions-on-words-as-u32.scm")

(load "padding-proc.scm")

(load "T.scm")

(load "bytevector-slice-proc.scm")

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

(load "word->num&num->word-like-list.scm")

(load "md5+md5<<<.scm")

(load "fourrounds.scm")

(load "4roundscalc.scm")

(load "padded-msg->512bits.scm")

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
      (yell-md5))));the calc passed without error but the answer is wrong:(
