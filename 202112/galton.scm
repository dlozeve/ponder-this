(import srfi-1 srfi-13)

(define (permute sigma k)
  (list-ref sigma k))

(define (score sigma dist)
  (define n (length dist))
  (define m (apply + dist))
  (let lp ((acc 0.0) (k 0))
    (if (= n k)
	acc
	(let ((a (list-ref dist (permute sigma k))))
	  (lp (+ acc (expt (/ (* n a) m) (add1 k))) (add1 k))))))

(define (string->symbol-list s)
  (map string->symbol (map string (string->list s))))

(define (string->pins s)
  (let lp ((pins '())
	   (k 1)
	   (s s))
    (if (string-null? s)
	(reverse pins)
	(lp (cons (string->symbol-list (string-take s k)) pins)
	    (+ 1 k)
	    (string-drop s k)))))

(define (pin-ref pins stage pos)
  (list-ref (list-ref pins stage) pos))

(define (pin-set! pins stage pos v)
  (set! (list-ref (list-ref pins stage) pos) v))

(define (drop-ball! pins)
  (let lp ((stage 0) (pos 0))
    (if (= stage (length pins))
	pos
	(let ((pin (pin-ref pins stage pos)))
	  (cond
	   ((eq? pin 'L) (begin
			   (pin-set! pins stage pos 'R)
			   (lp (+ 1 stage) pos)))
	   ((eq? pin 'R) (begin
			   (pin-set! pins stage pos 'L)
			   (lp (+ 1 stage) (+ 1 pos))))
	   ((eq? pin '<) (lp (+ 1 stage) pos))
	   ((eq? pin '>) (lp (+ 1 stage) (+ 1 pos))))))))

(define (drop-balls! pins n)
  (define dist (make-list (+ 1 (length (last pins))) 0))
  (let lp ((n n))
    (if (= n 0)
	dist
	(let* ((pos (drop-ball! pins))
	       (prev-count (list-ref dist pos)))
	  (set! (list-ref dist pos) (+ 1 prev-count))
	  (lp (- n 1))))))

(let ((pins (string->pins "RRRRRLLRLR"))
      (sigma '(0 1 4 3 2)))
  (score sigma (drop-balls! pins 15)))
