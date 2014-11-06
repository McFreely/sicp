(define (make-vect  x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vec)
  (cadr vec))

(define (add-vetc vec1 vec2)
  (make-vect (+ (xcor-vect vec1) (xcor-vect vec2))
             (+ (ycor-vect vec1) (ycor-vect vec2))))

(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec1) (xcor-vect vec2))
             (- (ycor-vect vec1) (ycor-vect vec2))))

(define (scale-vect vec scalar)
  (make-vect (* scalar (xcor-vect vec))
             (* scalar (ycor-vect vec))))

(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-cons origin edge1 edge2)
  cons origin (cons edge1 edge2))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (outline-painter frame)
  )
