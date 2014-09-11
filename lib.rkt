#lang typed/racket/base
(provide
 ℕ ℕ? ℤ ℤ? 𝔹 𝔹? ℝ ℝ? ℝ² ℝ²?
 ∅
 C
 snoc)
(require racket/set math/number-theory
         (for-syntax racket/base racket/syntax))

(define-syntax (define-type/pred stx)
  (syntax-case stx ()
    [(define-type/pred τ e)
     (with-syntax ([τ? (format-id #'τ "~a?" #'τ)])
       #'(begin (define-type τ e)
                (define-predicate τ? τ)))]))

(define-type/pred ℕ Natural)
(define-type/pred ℤ Integer)
(define-type/pred 𝔹 Boolean)
(define-type/pred ℝ Real)
(define-type/pred ℝ² (Pairof ℝ ℝ))

(define ∅ : (Setof Nothing) {set})

(: snoc : (∀ (X) (Listof X) X → (Listof X)))
(define (snoc xs x) (append xs (list x)))

(: C : ℕ ℕ → ℕ)
(define (C n k)
  (quotient (factorial n) (* (factorial k) (factorial (- n k)))))
