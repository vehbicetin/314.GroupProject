#lang racket
;Starter Assignment
;CMPE -314

;Formal Grammar

; Alphabet {+,-,*,**}
; T -> Terminal symbols (whole numbers)
; N -> NonTerminal symbols {S}
; S -> Start Point
; P -> Production Rules :
                  ; S -> number
                  ; S -> + S S
                  ; S -> - S S
                  ; S -> * S S
                  ; S -> ** S S
                  ; S -> (S)


#lang racket
(require plai-typed)
(print-only-errors #f)

;Data definition for prefix arithmetic expressions.
(define-type msl
;named each arithmetic expressions correspond to the formal grammar
; lhs -> left hand side
; rhs -> right hand side
; Each operation takes only two arguments
  [msl-num (n : number)]
  [msl-sum (lhs : msl) (rhs : msl)]
  [msl-subs (lhs : msl) (rhs : msl)]
  [msl-mult (lhs : msl) (rhs : msl)]
  [msl-exp (lhs : msl) (rhs : msl)]
  )
;Contract
;; eval msl -> number
;Purpose 
;; evaluate an msl expression

;; examples
;; (msl-num 7) -> 7
;; (msl-add (msl-num 3) (msl-num 4)) -> 7
;; (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)) -> 42

(define (evaluator [expr : msl])
(type-case msl expr
  [msl-num (n) n]
  [msl-sum (lhs rhs) (+ (evaluator lhs) (evaluator rhs))]
  [msl-subs (lhs rhs) (- (evaluator lhs) (evaluator rhs))]
  [msl-mult (lhs rhs) (* (evaluator lhs) (evaluator rhs))]
  [msl-exp (lhs rhs) (expt (evaluator lhs) (evaluator rhs))]
  ))

;Tests

(test (evaluator (msl-num 7))  7)
(test (evaluator (msl-num 12))  12)
(test (evaluator (msl-num 133))  133)
;Test for addition
(test (evaluator (msl-sum (msl-num 3) (msl-num 4)))  7)
(test (evaluator (msl-sum (msl-num 8) (msl-num 12)))  20)
(test (evaluator (msl-sum (msl-num 20) (msl-num 33)))  51) ; BAD one.
(test (evaluator (msl-sum (msl-num 33) (msl-num 40)))  73)
(test (evaluator (msl-sum (msl-sum (msl-num 3) (msl-num 4)) (msl-num 35)))  42)
;Test for subtraction
(test (evaluator (msl-subs (msl-num 10) (msl-num 7))) 3)
(test (evaluator (msl-subs (msl-num 22) (msl-num 13))) 9)
(test (evaluator (msl-subs (msl-num 71) (msl-num 70))) 1)
(test (evaluator (msl-subs (msl-num 22) (msl-num 9))) 12) ; BAD one.
(test (evaluator (msl-subs (msl-subs (msl-num 19) (msl-num 7)) (msl-num 6))) 6)
;Test for multiplication
(test (evaluator (msl-mult (msl-num 5) (msl-num 2))) 10)
(test (evaluator (msl-mult (msl-num 8) (msl-num 10))) 80)
(test (evaluator (msl-mult (msl-num 11) (msl-num 12))) 132)
(test (evaluator (msl-mult (msl-num 9) (msl-num 5))) 40) ; BAD one.
(test (evaluator(msl-mult (msl-mult (msl-num 5) (msl-num 2)) (msl-num 7))) 70)
;Test for multiplication
(test (evaluator (msl-exp (msl-num 4) (msl-num 3))) 64)
(test (evaluator (msl-exp (msl-num 5) (msl-num 4))) 625)
(test (evaluator (msl-exp (msl-num 8) (msl-num 4))) 6434) ; BAD one.
(test (evaluator (msl-exp (msl-num 10) (msl-num 3))) 1000)
(test (evaluator (msl-exp (msl-exp (msl-num 2) (msl-num 3)) (msl-num 2))) 64)
;Test for nested expressions
;(+ (-6 4) (* 2 7))
(test (evaluator (msl-sum (msl-subs (msl-num 6) (msl-num 4)) (msl-mult (msl-num 2) (msl-num 7)))) 16)
;(* (** 2 5) (- 12 8)) 
(test (evaluator (msl-mult (msl-exp (msl-num 2) (msl-num 5)) (msl-subs (msl-num 12) (msl-num 8)))) 128)

;; ------- END of Starter Assignment ----------- ;;

;; -------  ClassWork - Worksheet1 ----------- ;;

;; parse prefix s-expression -> msl
;; convert a prefix arithmetic expression into the equivalent msl form.
;; examples
;; '7 -> (msl-num 7)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(+ (+ 3 4) 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))

(define (pre-parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-sum (pre-parse (second sl)) (pre-parse (third sl)))]
         [(*) (msl-mult (pre-parse (second sl)) (pre-parse (third sl)))]
         [(-) (msl-subs (pre-parse (second sl)) (pre-parse (third sl)))]
         [(**) (msl-exp (pre-parse (second sl)) (pre-parse (third sl)))]

         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

;;Tests for pre-parse.
(pre-parse '(** 2 3))
(test (pre-parse '7) (msl-num 7))
(test (pre-parse '(+ 3 4)) (msl-sum (msl-num 3) (msl-num 4)))
(test (pre-parse '(+ (+ 3 4) 35)) (msl-sum (msl-sum (msl-num 3) (msl-num 4)) (msl-num 35)))

;; parse infix s-expression -> msl
;; convert a infix arithmetic expression into the equivalent msl form.
;; examples
;; '7 -> (msl-num 7)
;; '(3 + 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '((3 + 4) + 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))

(define (in-parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (second sl))
         [(+) (msl-sum (in-parse (first sl)) (in-parse (third sl)))]
         [(*) (msl-mult (in-parse (first sl)) (in-parse (third sl)))]
         [(-) (msl-subs (in-parse (first sl)) (in-parse (third sl)))]
         [(**) (msl-exp (in-parse (first sl)) (in-parse (third sl)))]

         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

; Tests for in-parse.
(in-parse '(3 + 4))
(test (in-parse '7) (msl-num 7))
(test (in-parse '(3 + 4)) (msl-sum (msl-num 3) (msl-num 4)))
(test (in-parse '((3 + 4) + 35)) (msl-sum (msl-sum (msl-num 3) (msl-num 4)) (msl-num 35)))


;unparser-infix
;Contract 
;msl -> s-expression
;This function takes msl type object and unparses it into infix arithmetic s-expression.
  (define (unparser-infix [expr : msl]) : s-expression
    (type-case msl expr
      (msl-num (n) (list (number->s-exp n)))
      (msl-sum (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '+))) (unparser-infix rhs)))
      (msl-mult (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '*))) (unparser-infix rhs)))
      (msl-subs (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '-))) (unparser-infix rhs)))
      (msl-exp (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '**))) (unparser-infix rhs)))
      ))
      
      

;unparser-prefix
;Contract 
;msl -> s-expression
;This function takes msl type object and unparses it into prefix arithmetic s-expression.
   (define (unparser-prefix [expr : msl]) : s-expression
     (type-case msl expr
       (msl-num (n) (list (number->s-exp n)))
       (msl-sum (lhs rhs) (append (list(symbol->s-exp '+)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       (msl-mult (lhs rhs) (append (list(symbol->s-exp '*)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       (msl-subs (lhs rhs) (append (list(symbol->s-exp '-)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       (msl-exp (lhs rhs) (append (list(symbol->s-exp '**)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       ))


(test (unparser-prefix (msl-mult (msl-num 3) (msl-num 5))) '(* 3 5))
(test (unparser-prefix (msl-mult (msl-mult (msl-num 3) (msl-num 5)) (msl-num 4))) '(* * 3 5 4))

(test (unparser-infix (msl-mult (msl-num 3) (msl-num 5))) '(3 * 5))
(test (unparser-infix (msl-sum (msl-mult (msl-num 3) (msl-num 5)) (msl-num 20))) '(3 * 5 + 20))
