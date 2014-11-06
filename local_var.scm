;; Previous Chapters approach
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
              balance)
      "Insufficient funds"))

;; Chapter 3 approach, with local state variable
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (> balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;; <make-withdraw> create "withdrawal processors"
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))     ;; W1 & W2 are completely independant
(define W2 (make-withdraw 100))     ;; objects, each with its own local
                                    ;; state

;; Simple "bank account object"
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;; Ex 3.1
(define (make-accumulator sum)
  (define (add-to-sum n)
    (begin (set! sum (+ sum n))
           sum))
  add-to-sum)

(define A (make-accumulator 5))

;; Ex 3.2
(define (make-monitored f)
  (let ((count 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) count)
            ((eq? m 'reset-count)
             (begin (set! count 0)
                    0))
            (else (begin (set! count (+ count 1))
                         (f m)))))
    mf))

(define s (make-monitored sqrt))

;; Ex 3.3
(define (make-account-pass balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insuficcient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch passphrase m)
    (cond ((not (eq? passphrase password)) (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request --MAKE-ACCOUNT" m))))
  dispatch)

;; Ex 3.4 To come back later
(define (make-account-pass balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insuficcient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch passphrase m)
    (cond ((not (eq? passphrase password))
           (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request --MAKE-ACCOUNT" m))))
  dispatch)

;; Monte Carlo simulation for estimating Pi
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (random 100000) (random 1000000)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
