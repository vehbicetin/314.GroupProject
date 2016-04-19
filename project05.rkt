;; Kenan Kural -111200007
;; CMPE - 314 

;; -------  Project 05 ----------- ;;

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
;; Function Application with multiple parameters.
  [appC (fun : symbol) (arg : ExprC)]
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
  [fdC (name : symbol) (arg : ExprC)  (body : ExprC)])

(fdC 'double  '(x  y) (plusC (idC  'x) (multC (idC  'y) (numC 2))))


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
(test (parse '(! 2)) (factC (numC 2)))


; Example function definition namespace.
(define FuncDefStore
  (list
   (fdC 'square 'x (parse '(* x x)))
   (fdC 'subC 'x (parse '(+ x -1)))
   (fdC 'negC 'x (parse '(* x -1)))
   (fdC 'double 'x (parse '(+ x x)))))



;; get-fundef : symbol (listof FunDefC) -> FunDefC
;; Purpose : To find given symbol's(function name/identifier) function definition
;; - from function definition namespace.

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))
;; Tests:
(test (get-fundef 'subC FuncDefStore) (fdC 'subC 'x (parse '(+ x -1))))
(test (get-fundef 'negC FuncDefStore) (fdC 'negC 'x (parse '(* x -1))))
(test (get-fundef 'square FuncDefStore) (fdC 'square 'x (parse '(* x x))))
 

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
     [appC (fun arg) (appC fun (subst what for arg))]
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
 "Tests for substitution"
 (test (subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) (plusC (plusC (numC 7) (numC 7)) (numC 7)))
 (test (subst(plusC (numC 3) (numC 4)) 'y (plusC (multC (idC  'y) (idC  'y)) (idC 'y))) (plusC (multC (plusC (numC 3) (numC 4)) (plusC (numC 3) (numC 4))) (plusC (numC 3) (numC 4))))
 (test (subst (numC 2) 'x (factC (idC 'x))) (factC (numC 2)))

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
   [appC (fun arg) (local ([define fd (get-fundef fun fds)])
               (interp (subst arg
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
 

 
 ;Tests for appC
 ;(interp(appC 'double (numC 7)) (fdC 'double  'x (plusC (idC  'x) (idC  'x))) )
 
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


;Tests for Project 04 two examples.

(fdC 'fa  '(x  y) (plusC (idC  'x) (multC (idC  'y) (numC 2))))
;(appC 'fa ((numC 2) (idC 'x)))
;(appC 'fa ((idC 'y) (numC 3)))

;The reason we have this problem because of the parameter names. There is a corruption which we use two different y but the program gets it as the same.
;We put one function inside another function, if we use for different names problem will be solved but there is another solution which we can check for corruptions
;before we start evaluating. We will go into details this week deeply.



;; Lazy Evaluation
;; ExprC fds -> number
;; Purpose -> it takes an expression and list of function definitions and return a number as output.
(define (interp2 [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error 'interp2 "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])
                 
               (interp2 (subst a (fdC-arg fd) (fdC-body fd)) fds))]
            
                
   [igz (exp1 exp2 exp3) (cond
                           [(> (interp2 exp1 fds) 0) (interp2 exp2 fds)]
                        [else (interp2 exp3 fds)])]
   [plusC (l r) (+ (interp2 l fds) (interp2 r fds))]
   [subC (l r) (- (interp2 l fds) (interp2 r fds))]
   [multC (l r) (* (interp2 l fds) (interp2 r fds))]
   [expC (l r) (expt (interp2 l fds) (interp2 r fds))]
   
   [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp2 (factC (- x 1)) fds))])]))


;; Eager Evaluation
;; ExprC fds -> number
;; Purpose -> it takes an expression and list of function definitions and return a number as output.
(define (interp3 [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error interp3 "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])

                 (interp3 (subst (numC (interp3 a fds))
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
   [igz (exp1 exp2 exp3) (cond
                           [(> (interp3 exp1 fds) 0) (interp3 exp2 fds)]
                        [else (interp3 exp3 fds)])]
   [plusC (l r) (+ (interp3 l fds) (interp3 r fds))]
   [subC (l r) (- (interp3 l fds) (interp3 r fds))]
   [multC (l r) (* (interp3 l fds) (interp3 r fds))]
   [expC (l r) (expt (interp3 l fds) (interp3 r fds))]
   [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp3 (factC (- x 1)) fds))])]))


;; Tests for lazy evaluation
(test (interp2 (numC 5) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 5)
(test (interp2 (numC 15) (fdC 'double  'x (plusC (idC 'x) (idC 'x)))) 15)
(test (interp2 (numC 2) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp2 (numC 7) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 7)
(test (interp2 (numC 55) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 55)

;; Tests for plus operation
(test (interp2 (plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 12)
(test (interp2 (plusC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 19)
(test (interp2 (plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 12)
(test (interp2 (plusC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 229)
(test (interp2 (plusC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 74)
 
;; Tests for subtraction operation
(test (interp2 (subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp2 (subC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 3)
(test (interp2 (subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp2 (subC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -29)
(test (interp2 (subC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -6)
 
;; Tests for multiplaction
(test (interp2 (multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp2 (multC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 88)
(test (interp2 (multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp2 (multC (numC 10) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1290)
(test (interp2 (multC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1360)
 
;; Tests for exponention operation
(test (interp2 (expC (numC 2) (numC 4)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 16)
(test (interp2 (expC (numC 11) (numC 2)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 121)
(test (interp2 (expC (numC 7) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 343)
(test (interp2 (expC (numC 10) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1000)
(test (interp2 (expC (numC 4) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 64)

;; Tests for igz (if greater than zero)
(test (interp2 (igz(numC 5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp2 (igz(numC -5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
(test (interp2 (igz(numC 55) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp2 (igz(numC 555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp2 (igz(numC -5555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
 

;; Tests for factorial operation
(test (interp2 (factC 1)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1)
(test (interp2 (factC 2)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp2 (factC 3)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 6)
(test (interp2 (factC 4)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 24)
(test (interp2 (factC 5)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 120) 


;;Tests for Eager evaluation
(test (interp3 (numC 5) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 5)
(test (interp3 (numC 15) (fdC 'double  'x (plusC (idC 'x) (idC 'x)))) 15)
(test (interp3 (numC 2) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp3 (numC 7) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 7)
(test (interp3 (numC 55) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 55)

;; Tests for plus operation
(test (interp3 (plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 12)
(test (interp3 (plusC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 19)
(test (interp3 (plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 12)
(test (interp3 (plusC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 229)
(test (interp3 (plusC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 74)
 
;; Tests for subtraction operation
(test (interp3 (subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp3 (subC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 3)
(test (interp3 (subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp3 (subC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -29)
(test (interp3 (subC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -6)
 
;; Tests for multiplaction
(test (interp3 (multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp3 (multC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 88)
(test (interp3 (multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp3 (multC (numC 10) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1290)
(test (interp3 (multC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1360)
 
;; Tests for exponention operation
(test (interp3 (expC (numC 2) (numC 4)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 16)
(test (interp3 (expC (numC 11) (numC 2)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 121)
(test (interp3 (expC (numC 7) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 343)
(test (interp3 (expC (numC 10) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1000)
(test (interp3 (expC (numC 4) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 64)


;; Tests for igz (if greater than zero)
(test (interp3 (igz(numC 5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp3 (igz(numC -5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
(test (interp3 (igz(numC 55) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp3 (igz(numC 555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp3 (igz(numC -5555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
 

;; Tests for factorial operation
(test (interp3 (factC 1)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1)
(test (interp3 (factC 2)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp3 (factC 3)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 6)
(test (interp3 (factC 4)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 24)
(test (interp3 (factC 5)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 120) 




;; λ-expression grammar
;; LAMBDA -> v
;; LAMBDA -> (LAMBDA LAMBDA)
;; LAMBDA -> (λ v LAMBDA)


;; λ-exp is an abstract syntax grammar or a parse tree definition for
;; λ-exp that defined above.
(define-type λ-exp
  (λ-sym (v : symbol))
  (λ-app (l : λ-exp)(r : λ-exp))
  (λ-def (v : symbol)(p : λ-exp)))

;; Tests:
(λ-sym 'x)
(λ-app (λ-sym 'x)(λ-sym 'y))
(λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))

;; parse : s-exp -> λ-exp
;; Purpose : To transform given s-expression to corresponding
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

;; Tests:
(test (parsel (symbol->s-exp 'y))(λ-sym 'y))
(test (parsel '(λ x x))(λ-def 'x (λ-sym 'x)))
(test (parsel '((λ x x) y))(λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
(test (parsel '((λ x x)(λ y y)))
      (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))
(test (parsel '(λ x (λ y (y x))))
      (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))


;; unparse : λ-exp -> s-exp
;; Purpose : To produce concrete syntax from given abstract syntax.
(define (unparse (le : λ-exp)) : s-expression
  (type-case λ-exp le
    (λ-sym (v) (symbol->s-exp v))
    (λ-app (l r)(list->s-exp (list (unparse l)(unparse r))))
    (λ-def (v p)(list->s-exp 
                 (list (symbol->s-exp 'λ)(symbol->s-exp v)(unparse p))))
    ))

;; Test:
(test (unparse (λ-sym 'y))(symbol->s-exp 'y))
(test (unparse (λ-def 'x (λ-sym 'x))) '(λ x x))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
               '((λ x x) y))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))
       '((λ x x)(λ y y)))
      
(test (unparse (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))
       '(λ x (λ y (y x))))



;; A set represented as a list.
;; union : (listof symbol) (listof symbol) -> (listof symbol)
;; finding the union of two sets.
(define (union (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
  (foldr (lambda (x y)
           (if (member x y)
               y
               (cons x y))) 
         empty
         (append s1 s2)))

;; set-difference : (listof symbol) (listof symbol) -> (listof symbol)
;; To find the set difference of two sets.
(define (set-difference (s1 : (listof symbol))  (s2 : (listof symbol))) : (listof symbol)
  (filter (lambda (x)
            (not (member x s2)))
          s1))

;; free-identifier : λ-calc -> (listof symbol)
;; Purpose : To find free identifiers in given λ expression.
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