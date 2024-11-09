(define initial-words
  '([A (uint-list->bytevector '(#x01 #x23 #x45 #x67) 'little 8)]
    [B (uint-list->bytevector '(#x89 #xab #xcd #xef) 'little 8)]
    [C (uint-list->bytevector '(#xfe #xbc #xda #x98) 'little 8)]
    [D (uint-list->bytevector '(#x76 #x54 #x32 #x10) 'little 8)]))

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
