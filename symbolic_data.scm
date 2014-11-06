(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; ex 2.54
(define (equal-prime? item1 item2)
  (cond ((null? item1) #t)
        ((not (eq? (car item1) (car item2))) #f)
        (else (equal-prime? (cdr item1) (cdr item2)))))
