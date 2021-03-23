#lang eopl

(define diff-one
  (lambda ()
    '(one)
    )
  )

(define diff-one?
  (lambda (l)
    (cond
      [(equal? l (diff-one)) #t]
      [(and (diff-one? (cadr l)) (diff-zero? (caddr l))) #t]
      [else #f]
      )
    )
  )

(define diff-zero
  (lambda ()
    '(diff (diff-one) (diff-one))
    )
  )

(define diff-zero?
  (lambda (l)
    (cond
      [(diff-one? l) #f]
      [(equal? l (diff-zero)) #t ]
      [(and (diff-one? (cadr l)) (diff-one? (caddr l))) #t]
      [(and (diff-negOne? (cadr l)) (diff-negOne? (caddr l))) #t]
      [(and (diff-zero? (cadr l)) (diff-zero? (caddr l))) #t]
      [else #f]
    )
    )
  )

(define diff-negOne
  (lambda ()
    '(diff (diff (one) (one)) (one))
    )
  )

(define diff-negOne?
  (lambda (l)
    (cond
      [(or (diff-one? l) (diff-zero? l)) #f]
      [(and (diff-zero? (cadr l)) (diff-one? (caddr l)) ) #t]
      [(and (diff-negOne? (cadr l)) (diff-zero? (caddr l)) ) #t]
      [else #f]
    )
    )
  )

(define diff
  (lambda (l)
    (cond
      [(diff-one? l) (diff-one)]
      ;[()]
      )
    )
  )