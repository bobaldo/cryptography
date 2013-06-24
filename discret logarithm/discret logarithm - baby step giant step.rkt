#lang racket

(require profile)

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

(define (log-expt base exp p)
  (letrec ((baz (lambda (b e a)
                  (cond ((= e 0) a)
                        ((odd? e) (baz b (- e 1) (modulo (* a b) p)))
                        ((even? e) (baz (modulo (* b b) p) (/ e 2) a))))))
    (baz base exp 1)))

(define (exp-mod base exp p)
  (cond ((> exp 0) (log-expt base exp p))
        (else (log-expt (inverse base p) (- exp) p))))

(define ht (make-hash))

(define (build-hash m a p)
  (letrec ((loop (lambda (x)
                   (if (< x m)
                       (begin
                         (hash-set! ht (exp-mod a x p) x)
                         (loop (+ x 1)))
                       ht))))
    (loop 0)))

;a generatore, b elemento del gruppo G di ordine n, p primo (ordine n-1)
;p può essere di qulsiasi problema ma con valore intorno a 10^10 l'algoritmo diventa lento
(define (baby-step-giant-step a b p)
  (letrec ((m (inexact->exact (ceiling (sqrt (- p 1)))))
           (hashmap (build-hash m a p))
           (alfa (log-expt a (- m) p))
           (loop (lambda (i gamma)
                   (if (< i m)
                       (if (hash-has-key? hashmap gamma)
                           (+ (* i m) (hash-ref hashmap gamma))
                           (loop (+ i 1) (modulo (* gamma alfa) p)))
                       #f))))
    (loop 0 b)))

;(time (baby-step-giant-step 3 57 113)) ; => 100, come da libro
;(baby-step-giant-step 5 4 47) ; => 36, come da esame (http://www.mat.uniroma2.it/~eal/Cr2009compito11.pdf)
;(baby-step-giant-step 2 5 1019) ; => 10, come da Wikipedia (http://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm_for_logarithms)
;(baby-step-giant-step 2 228 383) ; => 110, come da libro

(define (called)
  (map (lambda (x) (let-values (((res cpu real gc) (time-apply baby-step-giant-step (list 2 3 x))))
                      (list x real)))
       (list 200087 )))
; 2000303 20000159 200000447 2000000579 20000000687 200000000423 20000000000567 2000000000000447 

(define (printed)
  (letrec ((loop (lambda (r) 
                   (cond ((null? r) (display "end"))
                         (else (begin
                                 (display (format "~a ~a~%"(caar r) (cadar r)))
                                 (loop (rest r))))))))
    (loop (called))))

(define (foo)
  (baby-step-giant-step 2 3 200087))

(printed)