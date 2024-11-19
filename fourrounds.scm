(define-syntax round-1!
  (syntax-rules ()
    [(_ a b c d k s i X)
     (set! a (+ b (md5<<< (md5+ a (F b c d) (bytevector-u8-ref X k) (list-ref T (- i 1))) s)))]))

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
