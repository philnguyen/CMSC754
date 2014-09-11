#lang typed/racket/base
(require racket/set racket/list racket/match "lib.rkt")

(: S : (Sequenceof ℝ²) → (Sequenceof ℝ))
(define (S ps)
  (list->set
   (for*/list : (Listof ℝ) ([p ps] [q ps] #:unless (equal? p q))
     (/ (- (cdr p) (car p)) (- (cdr q) (car q))))))

(: range-count : (Listof ℝ²) → ℤ)
;; Count the number of nonnegative slopes in given list of distinct points
(define (range-count ps)
  (define P (sort ps (λ ([p : ℝ²] [q : ℝ²]) (< (car p) (car q)))))
  (define B (map (inst cdr ℝ ℝ) P))
  (define-values (_ I) (inversion-count B))
  (- (C (length ps) 2) I))

(: inversion-count : (Listof ℝ) → (Values (Listof ℝ) ℤ))
(define (inversion-count l)
  (match l
   [(or '() (list _)) (values l 0)]
   [_
    ; Solve for sublists
    (define-values (l₁ l₂) (split l))
    (define-values (B₁ I₁) (inversion-count l₁))
    (define-values (B₂ I₂) (inversion-count l₂))
    (define N₂ (length B₂))
    ; Merge sublists and compute I + I₁ + I₂
    (let go : (Values (Listof ℝ) ℤ) ([i 0] [j 0] [B₁ B₁] [B₂ B₂] [I 0] [M : (Listof ℝ) '()])
      (cond
       [(and (cons? B₁) (cons? B₂))
        (cond
         [(<= (car B₁) (car B₂)) (go (+ 1 i) j (cdr B₁) B₂ (+ I j) (cons (car B₁) M))]
         [else (go i (+ 1 j) B₁ (cdr B₂) I (cons (car B₂) M))])]
       [(cons? B₁) (values (append (reverse M) B₁) (+ I₁ I₂ I (* N₂ (length B₁))))]
       [else (values (append (reverse M) B₂) (+ I₁ I₂ I))]))]))

(: split : (Listof ℝ) → (Values (Listof ℝ) (Listof ℝ)))
(define (split l)
  ; Note: Splitting method matters for inversion counting
  (define n (length l))
  (define p (quotient n 2))
  (values (take l p) (drop l p)))
