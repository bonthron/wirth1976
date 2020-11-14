
;;; Straight Selection Sort

;;; Straight Selection is in some sense the opposite of straight insertion: Straight insertion
;;; considers in each step only the one next item of the source sequence and all items of the
;;; destination array to find the insertion point; straight selection considers all items of
;;; the source array to find the one withe least key and to deposit it as the one next item of
;;; the destination sequence. 

;;; This is not a stable sort. 
;;; O(n^2) 
;;; worst case: O(n^2) swaps 
;;; best case: O(1) swaps 

;;; We may conclude in general the algorithm of straight selection is to be preferred over straight insertion.


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

(define (rec-less-than a b) (< (rec-key a) (rec-key b)))


;;; --------------------------------------------------------------------------- selection-sort
;;; search a list for the smallest value beginning with 'start-at' index
;;; returns the position of the smallest value, not the acutal value

(define (smallest-index lst start-at)
  
  (define (inner x l i j)
    (if (null? l)
        i
        (if (and (> j i) (rec-less-than (car l) x))
            (inner (car l) (cdr l) j (+ j 1))
            (inner x (cdr l) i (+ j 1)))))

  (inner (nth start-at lst) lst start-at 0)
)


;;; --------------------------------------------------------------------------- selection-sort
;; O(n2)
;; smallest-index finds the smallest item and returns it's position
;; then all we need to do is swap the current i with the smallest item

(define (selection-sort lst)
  
  (define (inner i l)
    (if (= i (length l))
        l
        (inner (+ i 1) (swap-list-item l i (smallest-index l i)))))

  (inner 0 lst)
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


;; shuffle, then sort!
(selection-sort (fisher-yates-shuffle recs))
