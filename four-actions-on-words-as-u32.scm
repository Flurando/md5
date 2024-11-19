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
