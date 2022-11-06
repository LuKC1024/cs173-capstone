#lang racket
(require redex)
(define-language SMoLest
  ; expression
  [e ::=
     ; variable
     x
     ; value
     v
     ; addition
     (+ e ...)
     ; application
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
  extend : ((x any) ...)  (x ...) (any ...) -> ((x any) ...)
  [(extend ((x any) ...) (x_1 ...) (any_1 ...))
   ((x_1 any_1) ... (x any) ...)])

; an example of extend
(test-equal
 (term (extend ((a 1)) (b c) (2 3)))
 (term ((b 2) (c 3) (a 1))))



; lookup a value in the environment
(define-metafunction SMoLest
  lookup : ((any_k any_v) ...) any_n -> any
  [(lookup ((any_k any_v) ...) any_n)
   ,(letrec ([maybe-v (assoc (term any_n) (term ((any_k any_v) ...)))])
      (if maybe-v
          (last maybe-v)
          (error 'unbound "~e" (term any_n))))])

; an example of lookup
(test-equal
 (term (lookup ((a 1)) a))
 1)



; `eval` reduction relation
(define eval
  (reduction-relation
   SMoLest
   #:domain (e σ s)
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
        [(in-hole C v) σ_2 s])))

; example of evaluating +
(test-->> eval
          ; note the `()`, which denotes the empty environment
          (term [(+ (+ 1 2) (+ 3 4))
                 ()
                 ()])
          (term [10
                 ()
                 ()]))
; example of function calls
(test-->> eval
          ; note the `()`, which denotes the empty environment
          (term [(((lambda (x) (lambda (y) (+ x y))) 2) 3)
                 ()
                 ()])
          (term [5
                 ()
                 ()]))
(test-results)
