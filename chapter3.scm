(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? a (car lat)) (cdr lat))
	    (else (cons (car lat) (rember a (cdr lat)))))))))

(rember 'a '())
(rember 'bacon '(bacon lettuce and tomato))
(rember 'mint '(lambd chops and mint jelly))

(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l))
		 (firsts (cdr l)))))))
(firsts '((a b) (c d) (e f)))


(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
	    (else (cons (car lat) (insertR new old (cdr lat)))))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'jalapeno 'and '(tacos tamales and salsa))
(insertR 'e 'd '(a b c d f g d h))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old) (cons new lat))
	    (else (cons (car lat) (insertL new old (cdr lat)))))))))

(insertL 'a 'b '(a b c d))
(insertL 'a 'b '(c d b a b))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old) (cons new (cdr lat)))
	    (else (cons (car lat) (subst new old (cdr lat)))))))))
(subst 'topping 'fudge '(ice cream with fudge for dessera))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
	    (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))
(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))


(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) a) (multirember a (cdr lat)))
       (else (cons (car lat) (multirember a (cdr lat)))))))))
(multirember 'cup '(coffee cup tea cup and hick cup))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
       (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))
(multiinsertR 'a 'b '(a b b c d a))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
       (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))
(multiinsertL 'fried 'fish '(chips and fish or fish and fried))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
       (else (cons (car lat) (multisubst new old (cdr lat)))))))))
(multisubst 'a 'b '(a b a c a b a))
