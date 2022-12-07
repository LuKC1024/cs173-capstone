#lang racket
(require redex)

(define-language SMoLest
  ; observable
  [o ::=
     number
     (function)]
  ; expression
  [e ::=
     ; variable reference
     x
     ; value
     v
     ; addition
     (+ e ...)
     ; function call
     (e e ...)
     ; lambda
     (lambda (x ...) e)]
  ; value
  ; TODO: You are welcomed to change the definition of values
  [v ::=
     number
     (lambda (x ...) e σ)]
  ; environment
  [σ ::= ((x v) ...)]
  ; evaluation context
  ; used in `eval`
  ; these are the compound forms of `e`
  ; with one sub expression replaced by a hole, `C`.
  [C ::=
     (v ... C e ...)
     (+ v ... C e ...)
     hole]
  ; stack
  [s ::=
     ()
     (C σ s)]
  ; variable
  [x ::= variable-not-otherwise-mentioned])

; extend the environment with bindings to values
(define-metafunction SMoLest
  extend : ((x v) ...)  (x ...) (v ...) -> ((x v) ...)
  [(extend ((x_1 v_1) ...) (x_2 ...) (v_2 ...))
   ((x_2 v_2) ... (x_1 v_1) ...)])

; an example of extend
(test-equal
 (term (extend ((a 1)) (b c) (2 3)))
 (term ((b 2) (c 3) (a 1))))

; lookup a value in the environment
(define-metafunction SMoLest
  lookup : σ x -> v
  [(lookup σ x)
   ,(letrec ([maybe-v (assoc (term x) (term σ))])
      (if maybe-v
          (last maybe-v)
          (error 'unbound "~e" (term x))))])

; an example of lookup
(test-equal
 (term (lookup ((a 1)) a))
 1)

; `eval` reduction relation
(define eval
  (reduction-relation
   SMoLest
   ; TODO: You are welcomed to change the domain and other parts of this file accordingly
   #:domain (e σ s)
   ; +
   (--> [(in-hole C (+ number ...)) σ s]
        [(in-hole C ,(apply + (term (number ...)))) σ s])
   ; lookup
   (--> [(in-hole C x) σ s]
        [(in-hole C (lookup σ x)) σ s])
   ; lambda
   ; TODO: You are welcomed to change the lambda case
   (--> [(in-hole C (lambda (x ...) e)) σ s]
        [(in-hole C (lambda (x ...) e σ)) σ s])
   ; application
   (--> [(in-hole C ((lambda (x ...) e σ_1) v ...)) σ_2 s]
        [e (extend σ_1 (x ...) (v ...)) (C σ_2 s)])
   ; return
   (--> [v σ_1 (C σ_2 s)]
        [(in-hole C v) σ_2 s])
   ; TODO: Part 1. implement `eq?`
   ; TODO: Part 1. implement `if`
   ))

; TODO: Part 2. implement `let`
; TODO: Part 2. implement `let*`
; TODO: Part 2. implement `begin`

; example of evaluating +
(test-->> eval
          ; The first `()` denotes the empty environment
          ; The second `()` denotes the empty stack
          (term [(+ (+ 1 2) (+ 3 4))
                 ()
                 ()])
          (term [10
                 ()
                 ()]))
; You can use `stepper` to debug programs
; TODO: try uncomment the following expression or run it in the REPL
#;
(stepper eval
         ; The first `()` denotes the empty environment
         ; The second `()` denotes the empty stack
         (term [(+ (+ 1 2) (+ 3 4))
                ()
                ()]))


(define-metafunction SMoLest
  observe : v -> o
  [(observe number)
   number]
  ;; TODO: you are welcomed to change the lambda case
  [(observe (lambda (x ...) e σ))
   (function)])

(define-metafunction SMoLest
  run : e -> o
  [(run e)
   (observe v)
   (where ((v σ s)) ,(apply-reduction-relation* eval (term [e () ()])))])

(test-equal (term (run (+ (+ 1 2) (+ 3 4))))
            (term 10))
(test-equal (term (run (((lambda (x) (lambda (y) (+ x y))) 2) 3)))
            (term 5))
(test-equal (term (run ((lambda (x) (+ ((lambda (x) x) 3) x)) 1)))
            (term 4))
; TODO: uncomment and make sure your `eq?` works as we expected
#|
(test-equal (term (run (eq? (lambda (x) x)
                            (lambda (x) x))
                       ))
            (term #f))
(test-equal (term (run (let ([f (lambda (x) x)])
                         (eq? f f))
                       ))
            (term #t))
(test-equal (term (run (let* ([f (lambda (x) x)]
                              [g f])
                         (eq? ((lambda () g)) f))
                       ))
            (term #t))
|#
; TODO: uncomment and make sure your `if` works as we expected
#|
(test-equal (term (run (if (eq? 2 3))
                           ((lambda (x) (x x)) (lambda (x) (x x)))
                           42))
            (term 42))
|#
;; TODO: You are welcomed to add more tests here.
(test-results)
