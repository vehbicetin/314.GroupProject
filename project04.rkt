#lang racket

;; -------  Project 04 ----------- ;;

;Formal Grammar

; Alphabet {+,-,*,**,id, {,}}
; T -> Terminal symbols 
; N -> NonTerminal symbols {}
; S -> Start Point
; P -> Production Rules :
                  ; exp -> number
                  ; exp -> symbol
                  ; exp -> + exp exp
                  ; exp -> - exp exp
                  ; exp -> * exp exp
                  ; exp -> ** exp exp
                  ; exp -> -1 * exp (The best one without problem) (Uniary Minus)
                  ; exp -> (exp)

       ;; Function definition
       ; F is a function
       ; Ls is a list of parameters
       ; B is body
       ; F -> (Name)Ls{B}
       ; Name -> symbol
       ; B-> exp
       ; Ls-> listOfSymbols

       ;; Function Application
       ;Fa is function application
       ;Fs is a function symbol
       ;La is a list of arguments
       ;Fa -> FsLa
       ;La  -> listOfSymbols
       ;Fs -> symbol
     


(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
;; Function Application with multiple parameters.
  [appC (fun : symbol) (arg : (listof ExprC))]
  [plusC (l : ExprC) (r : ExprC)]
  [subC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [expC (l : ExprC) (r : ExprC)]
  [igz (exp1 : ExprC) (exp2 : ExprC) (exp3 : ExprC)]
  [factC (x : number)]
  )
;Extended data definition ,,Function data definition
;; Function Definition with multiple parameters. 
(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol))  (body : ExprC)])

;; parse : s-exp -> ExprC
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
          (appC (s-exp->symbol (first sl)) (parse (second sl)))]
         [else (error 'parse "invalid list input")])
       )]
    [else (error 'parse "invalid input")]))
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
;; it takes a symobol and generate a function definition.

(fdC 'double  '(x , y) (plusC (idC  'x) (idC  'y)))
;(fdC 'triple  'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x)))

   (define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
   (cond
     [(empty? fds) (error 'get-fundef "reference to undefined function")]
     [(cons? fds) (cond
                    [(equal? n (fdC-name (first fds))) (first fds)]
                    [else (get-fundef n (rest fds))])]))
 

 ;Contract
 ;; ExprC symbol ExprC -> ExprC
 ;Purpose
 ;; it takes a expression ( numC 7) , argument ('x) and the function it self. It produces the function with changes(numC 7) placed for every 'x in function
 ;;Examples
 ;;(subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) -> (plusC (plusC (numC 7) (numC 7)) (numC 7))
    (define (subst [what : (listof ExprC)] [for : (listof symbol)] [in : ExprC]) : ExprC
     (type-case ExprC in
     [numC (n) in]
     [idC (s) (cond
              [(symbol=? s for) what]
              [else in])]
     [appC (f a) (appC f (subst what for a))]
     [plusC (l r) (plusC (subst what for l)
                         (subst what for r))]
 
     [subC (l r) (plusC (subst what for l)
                         (subst what for r))]
     [multC (l r) (multC (subst what for l)
                         (subst what for r))]
     [expC (l r) (multC (subst what for l)
                         (subst what for r))]
     [factC (x) (factC (subst what for x))]
     [igz (exp1 exp2 exp3) (igz (subst what for exp1) (subst what for exp2) (subst what for exp3))]))
 ;Tests for substitution
 (test (subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) (plusC (plusC (numC 7) (numC 7)) (numC 7)))
 (test (subst(plusC (numC 3) (numC 4)) 'y (plusC (multC (idC  'y) (idC  'y)) (idC 'y))) (plusC (multC (plusC (numC 3) (numC 4)) (plusC (numC 3) (numC 4))) (plusC (numC 3) (numC 4))))
 ;(test (subst(numC 7 ,numC 8) '(x , y) (plusC (plusC (idC  'x) (idC  'y)) (idC 'x))) (plusC (plusC (numC 7) (numC 8)) (numC 7)))
 
 ;Contract
 ;;ExprC -> fds (listof FunDefC) - > number 
 ;Purpose
 ;;it takes an expression and list of function definitions and output a number (Function Application)
 ;;Examples
 ;(numC 7) (fdC 'double  'x (plusC (idC  'x) (idC  'x))) -> 7
 ;(igz(numC -5) (numC 1) (numC 0)) (fdC 'double  'x (plusC (idC  'x) (idC  'x))) -> 0
 (define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error 'interp "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])
               (interp (subst a
                              (fdC-arg fd)
                              (fdC-body fd))
                       fds))]
   [igz (exp1 exp2 exp3) (cond
                           [(> (interp exp1 fds) 0) (interp exp2 fds)]
                        [else (interp exp3 fds)])]
   [plusC (l r) (+ (interp l fds) (interp r fds))]
   [subC (l r) (- (interp l fds) (interp r fds))]
   [multC (l r) (* (interp l fds) (interp r fds))]
   [expC (l r) (expt (interp l fds) (interp r fds))]
   [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp (factC (- x 1)) fds))])]))
  
 
 ;Tests for interp
 ;Tests for num
 (test(interp(numC 7) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 7)
 (test(interp(numC 12) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 12)
 (test(interp(numC 33) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 13) ;BAD one.
 (test(interp(numC 42) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 42)
 (test(interp(numC 0) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 0)
 
 ;Test for idC
 ;(interp(idC 'x) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 
 
 ;Tests for appC
 ;(interp(appC ('double 'x)) (fdC 'double  'x (plusC (idC  'x) (idC  'x))))
 
 ;Tests for if greater than 0
 (test(interp(igz(numC 2) (numC 1) (numC 0)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 1)
 (test(interp(igz(numC -2) (numC 1) (numC 0)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 0)
 (test(interp(igz(numC 7) (numC 1) (numC 0)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 1)
 (test(interp(igz(numC 12) (numC 1) (numC 0)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 1)
 (test(interp(igz(numC -5) (numC 1) (numC 0)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 1) ;BAD one.
 
 ;Tests for plus operation
 (test (interp(plusC (numC 7) (numC 5)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 12)
 (test (interp(plusC (numC 11) (numC 8)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 19)
 (test (interp(plusC (numC 7) (numC 5)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 17) ;BAD one.
 (test (interp(plusC (numC 100) (numC 129)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 229)
 (test (interp(plusC (numC 34) (numC 40)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 74)
 
 ;Tests for subtraction operation
 (test (interp(subC (numC 7) (numC 5)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 2)
 (test (interp(subC (numC 11) (numC 8)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 3)
 (test (interp(subC (numC 7) (numC 5)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 7) ;BAD one.
 (test (interp(subC (numC 100) (numC 129)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) -29)
 (test (interp(subC (numC 34) (numC 40)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) -6)
 
 ;Tests for multiplaction
 (test (interp(multC (numC 7) (numC 5)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 35)
 (test (interp(multC (numC 11) (numC 8)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 88)
 (test (interp(multC (numC 7) (numC 5)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 17) ;BAD one.
 (test (interp(multC (numC 10) (numC 129)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 1290)
 (test (interp(multC (numC 34) (numC 40)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 1360)
 
 ;Tests for exponention operation
 (test (interp(expC (numC 2) (numC 4)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 16)
 (test (interp(expC (numC 11) (numC 2)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 121)
 (test (interp(expC (numC 7) (numC 3)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 7) ;BAD one.
 (test (interp(expC (numC 10) (numC 3)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 1000)
 (test (interp(expC (numC 4) (numC 3)) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 64)

 ;Tests for factorial operation
(test(interp(factC 4 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 24)
(test(interp(factC 5 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 120)
(test(interp(factC 3 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 6)
(test(interp(factC 2 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 2)
(test(interp(factC 1 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 0) ;BAD one.

;Tests for nested expressions
 (test (interp(igz(subC (numC 34) (numC 40)) (expC (numC 1) (numC 7)) (multC (numC 0) (numC 5))) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 0)
 (test (interp(igz(plusC (numC -5) (numC 10)) (multC (numC 1) (numC 1)) (subC (numC 4) (numC 4))) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 1)
