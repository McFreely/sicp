;; ex 2.59

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;; ex 2.60
;; In which duplicates are allowed

(define (element-of-set-dup? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set-multiple? x (cdr set)))))

(define (adjoin-set-dup x set) (cons x set))
;;

(define (union-set-dup set1 set2)
  (if (or (null? set1) (null? set2) '()
          (cons (car set1) (union-set (cdr set1) set2)))))
;; this version will be more efficient because we don't need to
;; check if an element is already present in the set
;; O(n)

(define (intersection-set-dup set1 set2))
;; I have trouble seeing how to implement an intersection that is
;; different than the first one.

;;;Sets as Ordered Lists
(define (element-of-set-ord? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set-ord? x (cdr set)))))

(define (intersection-set-ord set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (insersection-set-ord (cdr set1)
                                           (cdr set2))))
              ((< x1 x2)
               (intersection-set-ord (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-ord set1 (cdr set2)))))))

(define (adjoin-set-ord x set)
  (if (element-of-set-ord? x set)
      set
      (cons x set)))

(define (union-set-ord set1 set2))

;; Set as Binary Tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set-tree? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set-tree? x (left-branch set)))
        ((> x (entry set))
         (element-of-set-tree? x (right-branch set)))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-tree x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-tree x (right-branch set))))))
