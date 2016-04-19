#lang racket

;; -------  Project 02 ----------- ;;

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
                  ; S -> -1 * S (The best one without problem) (Uniary Minus)
                  ; S -> (S)
                 

(define-type FBAPEsug
  [numFS (n : number)]
  [plusFS (l : FBAPEsug) (r : FBAPEsug)]
  [subFS (l : FBAPEsug) (r : FBAPEsug)]
  [multFS (l : FBAPEsug) (r : FBAPEsug)]
  [expFS (l : FBAPEsug) (r : FBAPEsug)]
  [uminusFS (e : FBAPEsug)])

(define-type FBAPE
  [numF (n : number)]
  [plusF (l : FBAPE) (r : FBAPE)]
  [multF (l : FBAPE) (r : FBAPE)]
  [subF (l : FBAPE) (r : FBAPE)]
  [expF (l : FBAPE) (r : FBAPE)]
  [uminusF (e : FBAPE)]
  )

;; Parse prefix s-expression -> FBAPEsug
;; Convert a prefix arithmetic expression into the equivalent FBAPEsug form.
;; Examples
;; '7 -> (numFS 7)
;; '(+ 3 4) -> (plusFS (numFS 3) (numFS 4))
;; '(+ (+ 3 4) 35) -> (plusFS (plusFS (numFS 3) (numFS 4)) (numFS 35))

(define (preF-parse [s : s-expression]) : FBAPEsug
  (cond
    [(s-exp-number? s) (numFS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
      (cond
        [(= (length sl) 2) (case (s-exp->symbol (first sl)) [(-) (uminusFS (preF-parse (second sl)))]) ]
        [else (case (s-exp->symbol (first sl))
           [(+) (plusFS (preF-parse (second sl)) (preF-parse (third sl)))]
           [(*) (multFS (preF-parse (second sl)) (preF-parse (third sl)))]
           [(-) (subFS (preF-parse (second sl)) (preF-parse (third sl)))]
           [(**) (expFS (preF-parse (second sl)) (preF-parse (third sl)))]
         [else (error 'preF-parse "invalid list input")])]))]
    [else (error 'preF-parse "invalid input")]))

; Tests for preF-parse.

; Tests for numbers.
(test (preF-parse '7) (numFS 7))
(test (preF-parse '12) (numFS 17)) ;BAD one.
(test (preF-parse '71) (numFS 71))
(test (preF-parse '23) (numFS 23))
(test (preF-parse '5) (numFS 5))
;Tests for plus operation
(test (preF-parse '(+ 3 4)) (plusFS (numFS 3) (numFS 4)))
(test (preF-parse '(+ 7 48)) (plusFS (numFS 7) (numFS 48)))
(test (preF-parse '(+ 3 4)) (plusFS (numFS 3) (numFS 4)))
(test (preF-parse '(+ (+ 3 4) 3)) (plusFS (plusFS (numFS 3) (numFS 4)) (numFS 3)))
(test (preF-parse '(+ 3 4)) (plusF (numFS 3) (numF 4))) ;BAD one.
;Tests for uminus operation
(test (preF-parse '(- 7)) (uminusFS (numFS 7)))
(test (preF-parse '(- 12)) (uminusFS (numFS 12)))
(test (preF-parse '(- 23)) (uminusFS (numFS 23)))
(test (preF-parse '(- 44)) (uminusFS (numFS 44)))
(test (preF-parse '(- 668)) (uminusFS (numFS 301))) ;BAD one.
;Tests for exponention operation
(test (preF-parse '(** 2 5)) (expFS (numFS 2) (numFS 5)))
(test (preF-parse '(** 7 8)) (expFS (numFS 7) (numFS 8)))
(test (preF-parse '(** 3 7)) (expFS (numFS 3) (numFS 7)))
(test (preF-parse '(** 5 6)) (expFS (numFS 5) (numFS 6)))
(test (preF-parse '(** 12 2)) (expFS (numFS 2) (numFS 5))) ; BAD one.
;Tests for nested expressions
(test (preF-parse '(- (- 3) 5)) (subFS (uminusFS (numFS 3)) (numFS 5)))
(test (preF-parse '(+ (* (- 7) 5) 7)) (plusFS (multFS (uminusFS (numFS 7)) (numFS 5)) (numFS 7)))
(test (preF-parse '(** (+ (- 6) 15) 2)) (expFS (plusFS (uminusFS (numFS 6)) (numFS 15)) (numFS 2)))


;; Desugar FBAPEsug -> FBAPE
;; Convert a FBAPEsug expression into the equivalent FBAPE form.
;; Examples
;; (numFS 9) -> (numF 9)
;; (plusFS (numFS 7) (numFS 7)) -> (plusF (numF 7) (numF 7))
;; (uminusFS (numFS 7)) -> (multF (numF -1) (numF 7))


(define (desugar [as : FBAPEsug]) : FBAPE
  (type-case FBAPEsug as
    [numFS (n) (numF n)]
    [plusFS (l r) (plusF (desugar l)
                        (desugar r))]
    [multFS (l r) (multF (desugar l)
                        (desugar r))]
    [subFS (l r) (subF (desugar l)
                        (desugar r))]
    [expFS (l r) (expF (desugar l)
                      (desugar r))]
    
    [uminusFS (e) (desugar (multFS (numFS -1) e))]
))
;; Tests for desugaring procedure
(test (desugar(numFS 9)) (numF 9))
(test (desugar(numFS 17)) (numF 16)) ; BAD one.
(test (desugar(numFS 4)) (numF 4))
(test (desugar(numFS 112)) (numF 112))
(test (desugar(numFS 6)) (numF 6))
;; Tests for plus operation
(test (desugar(plusFS (numFS 8) (numFS 14))) (plusF (numF 8) (numF 14)))
(test (desugar(plusFS (numFS 16) (numFS 4))) (plusF (numF 16) (numF 4)))
(test (desugar(plusFS (plusFS (numFS 8) (numFS 14)) (numFS 8))) (plusF (plusF (numF 8) (numF 14)) (numF 8)))
(test (desugar(plusFS (numFS 6) (numFS 4))) (plusF (numF 6) (numF 4)))
(test (desugar(plusFS (numFS 12) (numFS 33))) (plusF (numF 12) (numF 30))) ;BAD one.
;; Tests for multiplication operation
(test (desugar(multFS (numFS 8) (numFS 14))) (multF (numF 8) (numF 14)))
(test (desugar(multFS (numFS 16) (numFS 4))) (multF (numF 16) (numF 4)))
(test (desugar(multFS (multFS (numFS 8) (numFS 14)) (numFS 8))) (multF (multF (numF 8) (numF 14)) (numF 8)))
(test (desugar(multFS (numFS 6) (numFS 4))) (multF (numF 6) (numF 4)))
(test (desugar(multFS (numFS 12) (numFS 33))) (multF (numF 12) (numF 30))) ;BAD one.
; Tests for subtraction operation
(test (desugar(subFS (numFS 8) (numFS 14))) (subF (numF 8) (numF 14)))
(test (desugar(subFS (numFS 16) (numFS 4))) (subF (numF 16) (numF 4)))
(test (desugar(subFS (subFS (numFS 8) (numFS 14)) (numFS 8))) (subF (subF (numF 8) (numF 14)) (numF 8)))
(test (desugar(subFS (numFS 6) (numFS 4))) (subF (numF 6) (numF 4)))
(test (desugar(subFS (numFS 12) (numFS 33))) (subF (numF 12) (numF 30))) ;BAD one.
; Tests for exponention operation
(test (desugar(expFS (numFS 8) (numFS 14))) (expF (numF 8) (numF 14)))
(test (desugar(expFS (numFS 16) (numFS 4))) (expF (numF 16) (numF 4)))
(test (desugar(expFS (expFS (numFS 8) (numFS 14)) (numFS 8))) (expF (expF (numF 8) (numF 14)) (numF 8)))
(test (desugar(expFS (numFS 6) (numFS 4))) (expF (numF 6) (numF 4)))
(test (desugar(expFS (numFS 12) (numFS 33))) (expF (numF 12) (numF 30))) ;BAD one.
; Tests for unaryminus operation
(test (desugar(uminusFS (numFS 7))) (multF (numF -1) (numF 7)))
(test (desugar(plusFS (uminusFS (numFS 10)) (uminusFS (numFS 7)))) (plusF (multF (numF -1) (numF 10)) (multF (numF -1) (numF 7))))
(test (desugar(uminusFS (numFS 11))) (multF (numF -1) (numF 11)))
(test (desugar(uminusFS (numFS 87))) (multF (numF -1) (numF 87)))
(test (desugar(uminusFS (numFS 72))) (multF (numF -1) (numF 71))) ;BAD one.

;Tests for nested expressions
;(+ (- 3) (* 5 (- (* 7 6)))))

(test (desugar (plusFS (uminusFS (numFS 3)) (multFS (numFS 5) (uminusFS (multFS (numFS 7) (numFS 6))))))
      (plusF (multF (numF -1) (numF 3)) (multF (numF 5) (multF (numF -1) (multF (numF 7) (numF 6))))))

;(* (5) (+ 10 (- (** 2 4)))))
(test (desugar (multFS (uminusFS (numFS 5)) (plusFS (numFS 10) (uminusFS (expFS (numFS 2) (numFS 4))))))
      (multF (multF (numF -1) (numF 5)) (plusF (numF 10) (multF (numF -1) (expF (numF 2) (numF 4))))))
