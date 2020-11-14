

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

(define (display-list l)
  (display (map (lambda (x) (rec-val x)) l))
  )


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


(define (key< a b) (< (rec-key a) (rec-key b)))
(define (key>= a b) (>= (rec-key a) (rec-key b)))


;;; --------------------------------------------------------------------------- sift
(define (sift lst L R)
  
  (define (inner i j x lst)
    (if (> j R)
	lst
	(begin
	  (if (< j R)
	      (if (key<
		   (nth j lst)
		   (nth (+ j 1) lst))
		  (set! j  (+ j 1))))

	  (if (key>= x (nth j lst))
	      lst
	      (inner
	       j
	       (+ 1 (* 2 j))
	       x
	       (replace-nth (replace-nth lst i (nth j lst)) j x))
	      ))))
  
  (let* ((i L)
	 (j  (+ 1 (* 2 i)))
	 (x (nth i lst)))
    (inner i j x lst))
  )



;;; --------------------------------------------------------------------------- heapsort
;;;  O(n log n)
;;;  This is not a stable sort.

(define (heapsort lst)
  
  (define (heapify lst L R)
    (if (< L 0)
	lst	  
	(heapify (sift lst L R) (- L 1) R)
	))
  
  (define (sift-down lst R)
    (if (< R 0)
	lst
	(let* ((first (nth 0 lst))
	       (last (nth R lst))
	       (swap-lst (replace-nth (replace-nth lst 0 last) R first))
	       (new-lst (sift swap-lst 0 (- R 1)))
	       )
	  (sift-down new-lst (- R 1))
	  )))
  
  (let* ((n (length lst))
	 (L (floor (/ n 2)))
	 (R (- n 1)))
    
    (sift-down (heapify lst L R) R)
    
    ))


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


(set! shuffled (fisher-yates-shuffle recs))

(display-list shuffled)
(newline)
(newline)

(display-list (heapsort shuffled))


