#lang racket
;(require profile)

(define (ex-gcd a b)
  (if (= (modulo a b) 0)
      (list 0 1)
      (let* ((res (ex-gcd b (modulo a b)))
             (x (car res))
             (y (cadr res)))
        (list y (- x (* y (floor (/ a b))))))))

(define (inverse a p)
  (first (ex-gcd a p)))

(define (square n)
  (expt n 2))

(define (log-expt b n p)
  (cond ((= n 0) 1)
        ((even? n) (modulo (square (log-expt b (/ n 2) p)) p))
        (else (modulo (* b (log-expt b (- n 1) p)) p))))

(define (exp-mod base exp p)
  (cond ((> exp 0) (log-expt base exp p))
        (else (log-expt (inverse base p) (- exp) p))))

(define (partition-s x)
  (cond ((= (modulo x 3) 0) 's2 )
        ((= (modulo x 3) 1) 's1 )
        ((= (modulo x 3) 2) 's3 )))

(define (define-range p)
  (/ (- p 1) 2))

;a generatore, b elemento del gruppo G di ordine n, p primo (ordine n-1)
;p deve essere della forma 2*q +1 ed eseremo q come ordine del gruppo n
(define (pollard-kangaroo alpha beta p)
  (letrec ((n (define-range p))
           (next-x (lambda (x)
                     (let* ((aux (partition-s x)))
                       (cond ((eq? aux 's1) (modulo (* beta x) p))
                             ((eq? aux 's2) (modulo (log-expt x 2 p) p))
                             ((eq? aux 's3) (modulo (* alpha x) p))))))
           (next-a (lambda (a x)
                     (let* ((aux (partition-s x)))
                       (cond ((eq? aux 's1) a)
                             ((eq? aux 's2) (modulo (* 2 a) n))
                             ((eq? aux 's3) (modulo (+ a 1) n))))))
           (next-b (lambda (b x)
                     (let* ((aux (partition-s x)))
                       (cond ((eq? aux 's1) (modulo (+ b 1) n))
                             ((eq? aux 's2) (modulo (* 2 b) n))
                             ((eq? aux 's3) b)))))
           (next-x-a-b (lambda (x a b)
                         (list (next-x x) (next-a a x) (next-b b x))))
           (next-x-a-b-double (lambda (x a b)
                                (let* ((aux (next-x-a-b x a b)))
                                  (next-x-a-b (first aux) (second aux) (third aux)))))
           (loop (lambda (x a b 2x 2a 2b)
                   (let* ((i (next-x-a-b x a b))
                          (2i (next-x-a-b-double 2x 2a 2b)))
                     ;(display (format "~a ~a ~a ~a ~a ~a \n" (first i) (second i) (third i) (first 2i) (second 2i) (third 2i)))
                     (if (= (first i) (first 2i))
                         (let* ((r (modulo (- (third i) (third 2i)) n)))
                           (if (= r 0) 
                               'errore
                               (modulo (* (exp-mod r -1 n) (- (second 2i) (second i))) n)))                    
                         (loop (first i) (second i) (third i) (first 2i) (second 2i) (third 2i)))))))
    (loop 1 0 0 1 0 0)))

;(pollard-kangaroo 3 58 113) ; => 100, come da libro
;(pollard-kangaroo 5 4 47) ; => 36, come da esame (http://www.mat.uniroma2.it/~eal/Cr2009compito11.pdf)
;(pollard-kangaroo 2 5 1019) ; => 10, come da Wikipedia (http://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm_for_logarithms)
;(pollard-kangaroo 2 228 383) ; => 110, come da libro 

(define (called)
  (map (lambda (x) (let-values (((res cpu real gc) (time-apply pollard-kangaroo (list 2 3 x))))
                      (list x real)))
       (list 200087)))
;  2000303 20000159 200000447 2000000579 20000000687 200000000423 20000000000567 2000000000000447 20000000000567

(define (printed)
  (letrec ((loop (lambda (r) 
                   (cond ((null? r) (display "end"))
                         (else (begin
                                 (display (format "~a ~a~%"(caar r) (cadar r)))
                                 (loop (rest r))))))))
    (loop (called))))

(define (foo)
  (pollard-kangaroo  2 3 200087))

(printed)