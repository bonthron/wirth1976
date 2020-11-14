

;;; --------------------------------------------------------------------------- list helpers
(define (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))


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
;; The Fisher-Yates shuffle, in its original form, was described in 1938 by Ronald Fisher and Frank Yates
;; in their book Statistical tables for biological, agricultural and medical research.
;; The modern version of the Fisher-Yates shuffle, designed for computer use, was introduced by Richard Durstenfeld
;; in 1964 and popularized by Donald E. Knuth in The Art of Computer Programming.
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


;;; --------------------------------------------------------------------------- rec record-type
(define-record-type rec (fields key val))

(define (rec-less-than i j lst) (< (rec-key (nth i lst)) (rec-key (nth j lst))))


;;; --------------------------------------------------------------------------- exchange-sort
;;; O(n2)
;;; aka bubblesort
;;; Exchange sort is inferior to both straight insertion and straight selection;
;;; in fact, the bubblesort has hardly anything to recommend it except its catchy name!

(define (exchange-pairs i lst)

   (define (inner j l)
     (if (= j 0)
   	l
   	(inner (- j 1) (if (rec-less-than j (- j 1) l)
   			(swap-list-item l (- j 1) j)
   			l))
   	))

   (inner i lst)
  )


(define (exchange-sort lst)

  (define (inner i l)
    (if (= i (length l))
        l
        (inner (+ i 1) (exchange-pairs i l))))
  
  (inner 1 lst)
  )



;; make some records

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


;; shuffle, then sort
(exchange-sort (fisher-yates-shuffle recs))
