(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (cond
       ((eq? a (car lat)) #t)
       (else (member? a (cdr lat))))))))
      

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     (else
      (cond
       ((member? (car lat) (cdr lat)) #f)
       (else (set? (cdr lat))))))))
(set? '(apple peaches apple plum))
(set? '(apples peaches pears plums))
(set? '())

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
       (else (cons (car lat) (makeset (cdr lat)))))))))
(makeset '(apple peach pear peach plum apple lemon peach))
(makeset '(apple 3 pear 4 9 apple 3 4))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))
(subset? '(5 chicken wings) '(5 hamburgers 2 piece fried chicken and light ducking wings))
(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

(define eqset?
  (lambda (set1 set2) (and (subset? set1 set2) (subset? set2 set1))))
(eqset? '(6 large chickens with wings) '(6 chickens with large wings))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (cond
       ((member? (car set1) set2) #t)
       (else (intersect? (cdr set1) set2)))))))
(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     (else
      (cond
       ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
       (else (intersect (cdr set1) set2)))))))
(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
      


