(define l '(Jack Sprat could eat no checken fat))
(lat? l)
(define l '((Jack) Sprat could eat no checken fat))
(lat? l)
(define l '(Jack (Sprat could) eat no checken fat))
(lat? l)
(define l '())
(lat? l)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))
(define l '(bacon and eggs))
(lat? l)
(or (null? '()) (atom '(d e f g)))
(or (null? '(a b c)) (null? '()))
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))
(member? 'tea '(coffee tea or milk))
(member? 'poache '(fried eggs and scrambled eggs))
