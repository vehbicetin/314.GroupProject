#lang racket
;; -------  Project 03 ----------- ;;

;Formal Grammar

; Alphabet {+,-,*,**,id}
; T -> Terminal symbols (whole numbers)
; N -> NonTerminal symbols {exp,statement,parameter,compoundstmt}
; S -> Start Point
; P -> Production Rules :
                  ; exp -> number
                  ; exp -> + exp exp
                  ; exp -> - exp exp
                  ; exp -> * exp exp
                  ; exp -> ** exp exp
                  ; exp -> -1 * exp (The best one without problem) (Uniary Minus)
                  ; exp -> (exp)

       ;; Function definition
                  ; statement -> id(parameter)
                  ; statement -> exp
                  ; parameter -> parameter
                  ; parameter -> id

       ;;if statement
                  ; statement -> if exp compundstmt
                  ; compundstmt -> {statement}

       ;;Function Application
                  ; expstatement -> number


(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [subC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [expC (l : ExprC) (r : ExprC)]
  
  [igz (exp1 : ExprC) (exp2 : ExprC) (exp3 : ExprC)]
  )
;Extended data definition ,,Function data definition
 
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])
;Contract
;; it takes one symbol -> fds : listOFFunDefC : FunDefC
;Purpose
;; it takes a symobol and generate a function definition.

(fdC 'double  'x (plusC (idC  'x) (idC  'x)))
(fdC 'triple  'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x)))

  (define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))


;(get-fundef 'double '((fdC 'double  'x (plusC (idC  'x) (idC  'x))) , (fdC 'triple  'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x)))))
;Contract
;; ExprC symbol ExprC -> ExprC
;Purpose
;; it takes a expression ( numC 7) , argument ('x) and the function it self. It produces the function with changes(numC 7) placed for ever 'x in function
;;Examples
;;(subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) -> (plusC (plusC (numC 7) (numC 7)) (numC 7))
   (define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
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
 
    [igz (exp1 exp2 exp3) (igz (subst what for exp1) (subst what for exp2) (subst what for exp3))]))
;Tests for substitution
(test (subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) (plusC (plusC (numC 7) (numC 7)) (numC 7)))
(test (subst(plusC (numC 3) (numC 4)) 'y (plusC (multC (idC  'y) (idC  'y)) (idC 'y))) (plusC (multC (plusC (numC 3) (numC 4)) (plusC (numC 3) (numC 4))) (plusC (numC 3) (numC 4))))


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
  [expC (l r) (expt (interp l fds) (interp r fds))]))
 

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

;Tests for nested expressions

(test (interp(igz(subC (numC 34) (numC 40)) (expC (numC 1) (numC 7)) (multC (numC 0) (numC 5))) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 0)
(test (interp(igz(plusC (numC -5) (numC 10)) (multC (numC 1) (numC 1)) (subC (numC 4) (numC 4))) (fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 1)