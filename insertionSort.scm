

;;; --------------------------------------------------------------------------- list helpers
(define (replace-nth list n item)
  (if (= n 0)
      (cons item (cdr list))
      (cons (car list) (replace-nth (cdr list) (- n 1) item))))


(define (swap-list-item list m n)
  (let
      ((a (list-ref list m))
       (b (list-ref list n)))
    (replace-nth
     (replace-nth list m b) n a)))


;;; --------------------------------------------------------------------------- fisherYatesShuffle
;;
;; The Fisher-Yates shuffle, in its original form, was described in 1938 by Ronald Fisher and Frank Yates in their book Statistical tables for biological, agricultural and medical research.
;; The modern version of the Fisher-Yates shuffle, designed for computer use, was introduced by Richard Durstenfeld in 1964 and popularized by Donald E. Knuth in The Art of Computer Programming.
;; O(n)

(define (fisher-yates-shuffle lst)
  (define (inner-shuffle lst i)
    (let* ((rand (random (- (length lst) i))))
      (if (> i 0)
          (inner-shuffle (swap-list-item lst i rand) (- i 1))
          lst)
      )
    )
  (inner-shuffle lst (- (length lst) 1))
  )


;;; --------------------------------------------------------------------------- insertion-sort
;; stable sort
;; O(n2)

(define (insert x lst predicate)
  (if (null? lst)
      (cons x '())
      (if (predicate x (car lst))
          (cons x lst)
          (cons (car lst) (insert x (cdr lst) predicate)))
      ))

(define (insertion-sort lst predicate)
  (if (null? lst)
      '()
      (insert (car lst) (insertion-sort (cdr lst) predicate) predicate)
      )
  )


;;; simple record type
(define-record-type rec (fields key val))

(set! recs (list
            (make-rec 0 "A")
            (make-rec 1 "B")
            (make-rec 2 "C")
            (make-rec 3 "D")
            (make-rec 4 "E")
            (make-rec 5 "F")
            (make-rec 6 "G")
            (make-rec 7 "H")
            (make-rec 8 "I")
            (make-rec 9 "J")
            ))


(define (rec-less-than a b) (< (rec-key a) (rec-key b)))

;; shuffle, then sort!
(insertion-sort (fisher-yates-shuffle recs) rec-less-than)



