;;;This file would be write with skeleton principle
;;;This file would work with 32bits integers instead of 4bytes bytevectors
;;;alone according to the wikipedia pseudocode
;;;try easier mode where only consider complete bytes input

#!
// : All variables are unsigned 32 bit and wrap modulo 2^32 when calculating
var int s[64], K[64]
var int i

// s specifies the per-round shift amounts
s[ 0..15] := { 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22 }
s[16..31] := { 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20 }
s[32..47] := { 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23 }
s[48..63] := { 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 }

// Use binary integer part of the sines of integers (Radians) as constants:
for i from 0 to 63 do
    K[i] := floor(232 × abs(sin(i + 1)))
end for
// (Or just use the following precomputed table):
K[ 0.. 3] := { 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee }
K[ 4.. 7] := { 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501 }
K[ 8..11] := { 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be }
K[12..15] := { 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821 }
K[16..19] := { 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa }
K[20..23] := { 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8 }
K[24..27] := { 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed }
K[28..31] := { 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a }
K[32..35] := { 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c }
K[36..39] := { 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70 }
K[40..43] := { 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05 }
K[44..47] := { 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665 }
K[48..51] := { 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039 }
K[52..55] := { 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1 }
K[56..59] := { 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1 }
K[60..63] := { 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391 }

// Initialize variables:
var int a0 := 0x67452301   // A
var int b0 := 0xefcdab89   // B
var int c0 := 0x98badcfe   // C
var int d0 := 0x10325476   // D

// Pre-processing: adding a single 1 bit
append "1" bit to message<    
 // Notice: the input bytes are considered as bit strings,
 //  where the first bit is the most significant bit of the byte.[53]

// Pre-processing: padding with zeros
append "0" bit until message length in bits ≡ 448 (mod 512)

// Notice: the two padding steps above are implemented in a simpler way
  //  in implementations that only work with complete bytes: append 0x80
  //  and pad with 0x00 bytes so that the message length in bytes ≡ 56 (mod 64).

append original length in bits mod 264 to message

// Process the message in successive 512-bit chunks:
for each 512-bit chunk of padded message do
    break chunk into sixteen 32-bit words M[j], 0 ≤ j ≤ 15
    // Initialize hash value for this chunk:
    var int A := a0
    var int B := b0
    var int C := c0
    var int D := d0
    // Main loop:
    for i from 0 to 63 do
        var int F, g
        if 0 ≤ i ≤ 15 then
            F := (B and C) or ((not B) and D)
            g := i
        else if 16 ≤ i ≤ 31 then
            F := (D and B) or ((not D) and C)
            g := (5×i + 1) mod 16
        else if 32 ≤ i ≤ 47 then
            F := B xor C xor D
            g := (3×i + 5) mod 16
        else if 48 ≤ i ≤ 63 then
            F := C xor (B or (not D))
            g := (7×i) mod 16
        // Be wary of the below definitions of a,b,c,d
        F := F + A + K[i] + M[g]  // M[g] must be a 32-bit block
        A := D
        D := C
        C := B
        B := B + leftrotate(F, s[i])
    end for
    // Add this chunk's hash to result so far:
    a0 := a0 + A
    b0 := b0 + B
    c0 := c0 + C
    d0 := d0 + D
end for

var char digest[16] := a0 append b0 append c0 append d0 // (Output is in little-endian)
!#

;;module-required (rnrs bytevectors)
;;usage -> {bytevector-u8-list, bytevector-length, bytevector->uint-list, bytevector-u8-set!, bytevector-copy!, bytevector-u32-set!} (procedure) <in> {pad, listify} (procedure)
;;
(use-modules (rnrs bytevectors))

;;syntax-name F
;;match -> (<$:self:F> B C D)
;;
(define-syntax F
  (syntax-rules ()
    [(_ B C D)
     (logior (logand B C) (logand (lognot B) D))]))

;;syntax-name G
;;match -> (<$:self:G> B C D)
;;
(define-syntax G
  (syntax-rules ()
    [(_ B C D)
     (logior (logand B D) (logand C (lognot D)))]))

;;syntax-name H
;;match -> (<$:self:H> B C D)
;;
(define-syntax H
  (syntax-rules ()
    [(_ B C D)
     (logxor B C D)]))

;;syntax-name I
;;match -> (<$:self:I> B C D)
;;
(define-syntax I
  (syntax-rules ()
    [(_ B C D)
     (logxor C (logior B (lognot D)))]))

;;procedure-name pad
;;input -> bv (bytevector)
;;output -> output-bv (bytevector) [(zero? (floor-remainder (bytevector-length output-bv) 64)) => #t]
;;
(define pad
  (lambda (bv)
    (let* ([bv-length (bytevector-length bv)]
	   [original-length (* 8 bv-length)]
	   
	   [remainder (floor-remainder bv-length 64)]
	   [pad-total (if (>= remainder 56) (- 128 remainder) (- 64 remainder))]
	   [total-length (+ bv-length pad-total)]
	   [output-bv (make-bytevector total-length 0)]
	   
	   [original-length-in-u32-bytevectors (let ([ans (make-bytevector 8)])
						 (bytevector-u32-set! ans 0 (logand #xFFFFFFFF (ash original-length -8)) (endianness little))
						 (bytevector-u32-set! ans 1 (logand #xFFFFFFFF original-length) (endianness little))
						 ans)])
      
      (bytevector-copy! bv 0 output-bv)
      (bytevector-u8-set! output-bv bv-length #x80)
      (bytevector-copy! original-length-in-u32-bytevectors 0 output-bv (- total-length 2) 2)

      output-bv)))

;;procedure-name listify
;;input -> bv (bytevector) [(zero? (floor-remainder (bytevector-length output-bv) 64)) => #t]
;;output -> _ (list:list:u32[16])
;;
(define listify
  (lambda (bv)
    (let ([bv-as-u32-list (bytevector->uint-list bv (endianness big) 4)])
      (let ([total-num (/ (length bv-as-u32-list) 16)]
	    (let loop ([n 0] [lst bv-as-u32-list)
		       (if (>= n (- total-num 1))
			   lst
			   (cons (list-head lst 16) (loop (+ 1 n) (list-tail lst 17)))))))))))

;;procedure-name process-512bits
;;input -> X (list;u32[16])
;;output -> <$:nil>
;;note -> "This function is intended to do the real 64 rounds calculation and update the A B C D in the end, but it doesn't need to loop at all"
;;note -> "Just use A B C D T as is"
;;caution -> "Due to the internal usage of A B C D T, this function should only be invoked inside procedure md5sum where A B C D T have already been set in the super lexical content"
;;
(define process-512bits
  (lambda (X)
    (let ([AA A][BB B][CC C][DD D]))))
      

;;procedure-name consentrated-128bits
;;input -> A B C D (u32)
;;output -> _ (u128)
;;
(define consentrated-128bits
  (lambda (A B C D)
    (+ (ash A 96)
       (ash B 64)
       (ash C 32)
       D)))

#! uncomment this block if guile complains about the unbound variable: format
;;module-required (ice-9 format)
;;usage -> format (procedure) <in> md5sum (procedure)
;;
(use-modules (ice-9 format))
!#

;;procedure-name md5sum
;;input -> bv (bytevector)
;;output -> _ (string)
;;
(define md5sum
  (lambda (bv)
    (let ([A #x67452301]
	  [B #xEFCDAB89]
	  [C #x98BADCFE]
	  [D #x10325476]
	  [T '(#xd76aa478, #xe8c7b756, #x242070db, #xc1bdceee, #xf57c0faf, #x4787c62a, #xa8304613, #xfd469501, #x698098d8, #x8b44f7af, #xffff5bb1, #x895cd7be, #x6b901122, #xfd987193, #xa679438e, #x49b40821, #xf61e2562, #xc040b340, #x265e5a51, #xe9b6c7aa, #xd62f105d, #x02441453, #xd8a1e681, #xe7d3fbc8, #x21e1cde6, #xc33707d6, #xf4d50d87, #x455a14ed, #xa9e3e905, #xfcefa3f8, #x676f02d9, #x8d2a4c8a, #xfffa3942, #x8771f681, #x6d9d6122, #xfde5380c, #xa4beea44, #x4bdecfa9, #xf6bb4b60, #xbebfbc70, #x289b7ec6, #xeaa127fa, #xd4ef3085, #x04881d05, #xd9d4d039, #xe6db99e5, #x1fa27cf8, #xc4ac5665, #xf4292244, #x432aff97, #xab9423a7, #xfc93a039, #x655b59c3, #x8f0ccc92, #xffeff47d, #x85845dd1, #x6fa87e4f, #xfe2ce6e0, #xa3014314, #x4e0811a1, #xf7537e82, #xbd3af235, #x2ad7d2bb, #xeb86d391)]
	  [s '(7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22, 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20, 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23, 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21)])
      (let* ([padded-bv (pad bv)]
	     [512bits-word-lists (listify padded-bv)])
	(for-each process-512bits 512bits-word-lists)
	(format #f "~32,'0x" (consentrated-128bits A B C D))))))
      
