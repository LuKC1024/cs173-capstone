#lang racket
(require redex)

#|
You must keep the definition of observable unchanged.
You must keep all existing kinds of expressions,
but you are welcomed to add new kinds, such as `eq?`.
You must keep the `run` tests unchanged.

You are welcomed to change everything else.

Some hints are provided in this file.
You can find them by searching "HINT".
|#

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
   #:domain [e σ s]
   ; +
   (--> [(in-hole C (+ number ...)) σ s]
        [(in-hole C ,(apply + (term (number ...)))) σ s])
   ; lookup
   (--> [(in-hole C x) σ s]
        [(in-hole C (lookup σ x)) σ s])
   ; lambda
   (--> [(in-hole C (lambda (x ...) e)) σ s]
        [(in-hole C (lambda (x ...) e σ)) σ s])
   ; application
   (--> [(in-hole C ((lambda (x ...) e σ_1) v ...)) σ_2 s]
        [e (extend σ_1 (x ...) (v ...)) (C σ_2 s)])
   ; return
   (--> [v σ_1 (C σ_2 s)]
        [(in-hole C v) σ_2 s])
   ))

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
; HINT: You are welcomed to add more tests here.
; HINT: try uncomment the following expression or run it in the REPL.
;   You might find `stepper` a very useful debugger
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
  [(observe (lambda (x ...) e σ))
   (function)])

(define (run e)
  (define results (apply-reduction-relation* eval (term [,e () ()])))
  (if ((redex-match? SMoLest ((v σ s a))) results)
      (first (first results))
      (error 'run "For details, run\n(stepper eval (term [~a () ()]))" e)))

(test-equal (run (term (+ (+ 1 2) (+ 3 4))))
            (term 10))
(test-equal (run (term (((lambda (x) (lambda (y) (+ x y))) 2) 3)))
            (term 5))
(test-equal (run (term ((lambda (x) (+ ((lambda (x) x) 3) x)) 1)))
            (term 4))
; HINT: uncomment and make sure your `eq?` works as we expected
#|
(test-equal (run (term (eq? (lambda (x) x)
                            (lambda (x) x))
                       ))
            (term #f))
(test-equal (run (term (let ([f (lambda (x) x)])
                         (eq? f f))
                       ))
            (term #t))
(test-equal (run (term (let* ([f (lambda (x) x)]
                              [g f])
                         (eq? ((lambda () g)) f))
                       ))
            (term #t))
|#
; HINT: uncomment and make sure your `if` works as we expected
#|
(test-equal (run (term (if (eq? 2 3)
                           ((lambda (x) (x x)) (lambda (x) (x x)))
                           42)))
            (term 42))
|#
; HINT: You are welcomed to add more tests here.
(test-results)
