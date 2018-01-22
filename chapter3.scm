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
(insertR 'e 'd '(a b c d f g d h)
   
