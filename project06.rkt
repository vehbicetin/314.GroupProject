;; Project 06 by Kenan Kural
;; 111200007


#lang racket
(require plai-typed)
(print-only-errors #f)

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
; Function Application with multiple parameters.
  [appC (f : symbol) (a : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [subC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [expC (l : ExprC) (r : ExprC)]
  [igz (exp1 : ExprC) (exp2 : ExprC) (exp3 : ExprC)]
  [ifZeroC (pred : ExprC)  (t : ExprC) (f : ExprC)]
  [factC (x : number)]
  [fibC (n : number)]
  )
;Extended data definition ,,Function data definition
; Function Definition with multiple parameters. 
(define-type FunDefC
  [fdC (name : symbol) (arg : ExprC)  (body : ExprC)])

;; Parse : s-exp -> ExprC
;; Purpose : To parse given s-exp to ExprC form
(define (parse [s :  (listof s-expression)]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)]) 
       (cond
         [(= (length sl) 4)
          (if (symbol=? 'igz (s-exp->symbol (first sl)))
              (igz (parse (second sl))
                       (parse (third sl))
                       (parse (fourth sl)))
              (error 'parse "invalid expression as input"))]
         [(= (length sl) 3)
          (case (s-exp->symbol (first sl))
            [(+) (plusC (parse (second sl)) (parse (third sl)))]
            [(*) (multC (parse (second sl)) (parse (third sl)))]
            [(-) (subC (parse (second sl)) (parse (third sl)))]
            [(**) (expC (parse (second sl)) (parse (third sl)))]
            [else (error 'parse "invalid list input")]
            )]
         [(= (length sl) 2)
          (case (s-exp->symbol (first sl))
          [(f) (appC (s-exp->symbol (first sl)) (parse (second sl)))]
          [(!) (factC (parse (second sl)))]   
          [else (error 'parse "invalid list input")])]
       ))
        ]))

;; Tests :
"tests"
(test (parse (number->s-exp 5))(numC 5))
(test (parse (symbol->s-exp 'x))(idC 'x))
(test (parse '(+ 3 4))(plusC (numC 3)(numC 4)))
(test (parse '(- 3 4))(subC (numC 3)(numC 4)))
(test (parse '(** 3 4))(expC (numC 3)(numC 4)))
(test (parse '(* 3 4))(multC (numC 3)(numC 4)))
(test (parse '(+ x x))(plusC (idC 'x)(idC 'x)))
(test (parse '(* x x))(multC (idC 'x)(idC 'x)))
(test (parse '(f (* x x)))(appC 'f (multC (idC 'x)(idC 'x))))
(test (parse '(igz 4 5 6))(igz (numC 4)(numC 5)(numC 6)))


;Contract
;;symbol (list of func definitions)-> : FunDefC
;Purpose
;; it takes a symobol and find the appopriate function for this symbol(name of the function itself).
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
   (cond
     [(empty? fds) (error 'get-fundef "reference to undefined function")]
     [(cons? fds) (cond
                    [(equal? n (fdC-name (first fds))) (first fds)]
                    [else (get-fundef n (rest fds))])]))
 

;; Binding is a data type to match value with identifiers.
(define-type Binding
  [bind (name : symbol) (val : number)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty) ;;--> empty envorionment
(define extend-env cons)

;; lookup : symbol (listof Bindings) -> number
;; Purpose : To find given symbol's value from environment.
;; Differ from substitution, not the search for all just for neccessary ones.
(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))


;; interp : ExprC (listof FunDefC) env -> number
;; Purpose : To evaluate expressions to numbers with multiple parameters.

(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                          (extend-env (bind (fdC-arg fd)
                  (interp a env fds))
            mt-env)
                          fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [subC (l r) (- (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    [expC (l r) (expt (interp l env fds) (interp r env fds))]
    [ifZeroC (pred t f)
             (if (= 0 (interp pred env fds))
                 (interp t env fds)
                 (interp f env fds))]
    [igz (exp1 exp2 exp3)
             (if (< 0 (interp exp1 env fds))
                 (interp exp2 env fds)
                 (interp exp3 env fds))]
    
    [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp (factC (- x 1)) env fds))])]
    [fibC (n) (cond
                [(= n 0) 1]
                [(= (- n 1) 0) 1]
                [(= (- n 2) 0) 1]
                [else (+ (interp(fibC (- n 1)) env fds) (interp(fibC (- n 2)) env fds))])]
                
   
    ))
;; Tests for fibonacci
(test(interp (fibC 1) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1)
(test(interp (fibC 2) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1)
(test(interp (fibC 3) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test(interp (fibC 4) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 3)
(test(interp (fibC 5) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 5)



;; Tests for factorial operation
(test (interp (factC 1) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1)
(test (interp (factC 2) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp (factC 3) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 6)
(test (interp (factC 4) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 24)
(test (interp (factC 5) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 120) 


;; Tests for envorionment interpreter 
(test (interp (numC 5) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 5)
(test (interp (numC 15) mt-env (fdC 'double  'x (plusC (idC 'x) (idC 'x)))) 15)
(test (interp (numC 2) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp (numC 7) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 7)
(test (interp (numC 55) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 55)

;; Tests for plus operation
(test (interp (plusC (numC 7) (numC 5)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 12)
(test (interp (plusC (numC 11) (numC 8)) mt-env (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 19)
(test (interp (plusC (numC 7) (numC 5)) mt-env (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 12)
(test (interp (plusC (numC 100) (numC 129)) mt-env (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 229)
(test (interp (plusC (numC 34) (numC 40)) mt-env (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 74)
 
;; Tests for subtraction operation
(test (interp (subC (numC 7) (numC 5)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp (subC (numC 11) (numC 8)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 3)
(test (interp (subC (numC 7) (numC 5)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp (subC (numC 100) (numC 129)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -29)
(test (interp (subC (numC 34) (numC 40)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -6)
 
 ;; Tests for multiplaction
(test (interp (multC (numC 7) (numC 5)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp (multC (numC 11) (numC 8)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 88)
(test (interp (multC (numC 7) (numC 5)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp (multC (numC 10) (numC 129)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1290)
(test (interp (multC (numC 34) (numC 40)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1360)
 
 ;;Tests for exponention operation
(test (interp (expC (numC 2) (numC 4)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 16)
(test (interp (expC (numC 11) (numC 2)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 121)
(test (interp (expC (numC 7) (numC 3)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 343)
(test (interp (expC (numC 10) (numC 3)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1000)
(test (interp (expC (numC 4) (numC 3)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 64)

;;Tests for ifZero
(test (interp (ifZeroC(numC 5) (numC 1) (numC 0)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
(test (interp (ifZeroC(numC 0) (numC 1) (numC 0)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp (ifZeroC(numC 0) (numC 1) (numC 0)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp (ifZeroC(numC 0) (numC 1) (numC 0)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp (ifZeroC(numC -5) (numC 1) (numC 0)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)


;;Tests for igz (if greater than zero)
(test (interp (igz(numC 5) (numC 1) (numC 0)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp (igz(numC 0) (numC 1) (numC 0)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
(test (interp (igz(numC 55) (numC 1) (numC 0)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp (igz(numC 555) (numC 1) (numC 0)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp (igz(numC -5) (numC 1) (numC 0)) mt-env (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
 



; λ-expression grammar
; LAMBDA -> v
; LAMBDA -> (LAMBDA LAMBDA)
; LAMBDA -> (λ v LAMBDA)


; λ-exp is an abstract syntax grammar or a parse tree definition for
; λ-exp that defined above.
(define-type λ-exp
  (λ-sym (v : symbol))
  (λ-app (l : λ-exp)(r : λ-exp))
  (λ-def (v : symbol)(p : λ-exp)))

; Tests:
(λ-sym 'x)
(λ-app (λ-sym 'x)(λ-sym 'y))
(λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))

; parse : s-exp -> λ-exp
; Purpose : To transform given s-expression to corresponding
(define (parsel (sexp : s-expression)) : λ-exp
  (cond
    [(s-exp-symbol? sexp)(λ-sym (s-exp->symbol sexp))]
    [(s-exp-list? sexp)
     (let ([sexp-list (s-exp->list sexp)])
       (cond
         [(= 2 (length sexp-list))
          (λ-app (parsel (first sexp-list))(parsel (second sexp-list)))]
         [(= 3 (length sexp-list))
          (if (and (symbol=? 'λ (s-exp->symbol (first sexp-list)))
                   (s-exp-symbol? (second sexp-list)))
              (λ-def (s-exp->symbol(second sexp-list))
                     (parsel (third sexp-list)))
              (error parsel "Not valid λ-definition")
              )]
         [else (error parsel "Not valid length λ-exp")]
         ))]
    [else (error parsel "Not valid λ-exp")]
))

; Tests:
(test (parsel (symbol->s-exp 'y))(λ-sym 'y))
(test (parsel '(λ x x))(λ-def 'x (λ-sym 'x)))
(test (parsel '((λ x x) y))(λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
(test (parsel '((λ x x)(λ y y)))
      (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))
(test (parsel '(λ x (λ y (y x))))
      (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))


; unparse : λ-exp -> s-exp
; Purpose : To produce concrete syntax from given abstract syntax.
(define (unparse (le : λ-exp)) : s-expression
  (type-case λ-exp le
    (λ-sym (v) (symbol->s-exp v))
    (λ-app (l r)(list->s-exp (list (unparse l)(unparse r))))
    (λ-def (v p)(list->s-exp 
                 (list (symbol->s-exp 'λ)(symbol->s-exp v)(unparse p))))
    ))

 ;Test:
(test (unparse (λ-sym 'y))(symbol->s-exp 'y))
(test (unparse (λ-def 'x (λ-sym 'x))) '(λ x x))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
               '((λ x x) y))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))
       '((λ x x)(λ y y)))
      
(test (unparse (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))
       '(λ x (λ y (y x))))



; A set represented as a list.
; union : (listof symbol) (listof symbol) -> (listof symbol)
; finding the union of two sets.
(define (union (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
  (foldr (lambda (x y)
           (if (member x y)
               y
               (cons x y))) 
         empty
         (append s1 s2)))

; set-difference : (listof symbol) (listof symbol) -> (listof symbol)
; To find the set difference of two sets.
(define (set-difference (s1 : (listof symbol))  (s2 : (listof symbol))) : (listof symbol)
  (filter (lambda (x)
            (not (member x s2)))
          s1))

; free-identifier : λ-calc -> (listof symbol)
; Purpose : To find free identifiers in given λ expression.
(define (free-identifier (le : λ-exp)) : (listof symbol)
  (type-case λ-exp le
    (λ-sym (v) (list v))
    (λ-app (l r)(union 
                 (free-identifier l)
                 (free-identifier r)))
    (λ-def (v p)(set-difference (free-identifier p)
                                (list v)))
    ))
(test (free-identifier (parsel '(λ x x))) empty)
(test (free-identifier (parsel '(λ x y))) (list 'y))
(test (free-identifier (parsel '((λ x y)(λ y z)))) (list 'y 'z))
(test (free-identifier (parsel '((λ f y)(λ z z)))) (list 'y))
