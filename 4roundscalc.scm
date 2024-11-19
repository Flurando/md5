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
