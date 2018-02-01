(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(pair? 'a)
(pair? '(a b))
(pair? (cons 'a 'b))

(null? 'a)
(null? '())

(atom? 14)

(define add1 (lambda (x) (+ x 1)))
(define sub1 (lambda (x) (- x 1)))

(add1 13)
(sub1 73)
(sub1 0)
(zero? 0)
(zero? 1)

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))
(o+ 2 0)
(o+ 1 4)

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))
(o- 3 3)
(o- 6 2)

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup) (addtup (cdr tup)))))))
(addtup '(1 4 2))

(define times
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ (times n (sub1 m)) n)))))
(times 3 2)
(times 10 12)

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) '())
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ '(1 2) '(3 4))
(tup+ '(3 5 2 6) '(2 4 5))

(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (> (sub1 n) (sub1 m))))))
(> 0 0)
(> 3 1)
(> 1 3)

(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (< (sub1 n) (sub1 m))))))
(< 0 0)
(< 3 1)
(< 1 3)

(define =
  (lambda (n m)
    (cond
     ((< n m) #f)
     ((> n m) #f)
     (else #t))))
(= 1 3)
(= 0 0)
(= 3 3)

(define pow
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (times (pow n (sub1 m)) n)))))
(pow 2 2)
(pow 3 1)
(pow 3 3)

(define div
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (div (o- n m) m))))))
(div 5 2)
(div 10 3)


(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))
(length '(a b c d e))

(define pic
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pic (sub1 n) (cdr lat))))))
(pic 1 '(a b))
(pic 3 '(a b c d e))
(pic 0 '(a))

(define rempic
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) (rempic (sub1 n) (cdr lat)))))))
(rempic 3 '(hotdogs with hot mustard))

(number? 1)
(number? 'a)
(number? '(1 2 3))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (no-nums (cdr lat)))
       (else (cons (car lat) (no-nums (cdr lat)))))))))
(no-nums '(5 pears 6 prums 9 dates))
 
