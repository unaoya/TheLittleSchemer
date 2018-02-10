(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) (quote +))
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote *))
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote ^))
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp)))))))))

(numbered? 1)
(numbered? '(1 + 2))
(numbered? '(3 + (4 ^ 5)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) (quote +)) (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) (quote *)) (times (value (car nexp)) (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) (quote ^)) (pow (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

(value 4)
(value '(3 + 4))
(value '(2 * (2 ^ 2)))

(define fst-sub-exp
  (lambda (aexp) (car (cdr aexp))))
(fst-sub-exp '(+ 2 3))

(define snd-sub-exp
  (lambda (aexp) (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp) (car aexp)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote +)) (o+ (value (fst-sub-exp nexp)) (value (snd-sub-exp nexp))))
     ((eq? (operator nexp) (quote *)) (times (value (fst-sub-exp nexp)) (value (snd-sub-exp nexp))))
     ((eq? (operator nexp) (quote ^)) (pow (value (fst-sub-exp nexp)) (value (snd-sub-exp nexp)))))))

(value 3)
(value '(+ 2 3))
(value '(* 2 (^ 2 2)))

(define sero?
  (lambda (n)
    (null? n)))
(sero? '())
(sero? 0)

(define edd1
  (lambda (n)
    (cons '() n)))
(edd1 '())
(edd1 (edd1 (edd1 '())))


(define zub1
  (lambda (n)
    (cdr n)))
(zub1 '(() () ()))

(define o+
  (lambda (n m)
    (cond
     ((null? m) n)
     (else (edd1 (o+ n (zub1 m)))))))

(o+ '(() ()) '(() () ()))
