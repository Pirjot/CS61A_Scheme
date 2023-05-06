(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
    ;cons-all takes in an element first and a list of lists rests, and adds first to the beginning of each list in rests
  (map (lambda (lst) (append `(,first) lst)) rests)
  )

(define (zip pairs)
    `(,(map car pairs) ,(map car (map cdr pairs)))
  )

;; Problem 16
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
    (define (helper index sofar lst)
        (if (null? lst) sofar
               (helper (+ index 1) (append sofar `((,index ,(car lst)))) (cdr lst))
        )
    )
    (helper 0 '() s)
)
  ; END PROBLEM 16

;; Problem 17
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 17
  ; Needs to be optimized for editor
  (cond ((or (null? denoms) (< total 0) (= total 0)) nil)
        ; ((= total 0) `((,(car denoms))))
        ((< total (car denoms)) (list-change total (cdr denoms)))
        ((= total (car denoms)) (append `((,(car denoms))) (list-change total (cdr denoms))))
        (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
  )
  ; END PROBLEM 17
)
;; Problem 18
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           ;(lambda (x y) x)
           (if (null? (cdr (cddr expr))) `(,form ,params ,(car body))
               `(,form ,params ,(car body) ,(let-to-lambda (cadr body)))
           )
           ; END PROBLEM 18
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (append `((lambda ,(car (zip values)) ,(let-to-lambda (car body)))) (map let-to-lambda (cadr (zip values))))
           ; END PROBLEM 18
           ))
        (else
         ; BEGIN PROBLEM 18
         `(,(car expr) ,(let-to-lambda (cadr expr)) ,(let-to-lambda (car (cddr expr))))
         ; END PROBLEM 18
         )))