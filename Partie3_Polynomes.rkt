#lang scheme

;; QUESTION N°1 ;;
(define polQ1
  (cons (cons 5 (cons 2 '())) (cons (cons 2 (cons 0 '())) (cons (cons -3 (cons 1 '())) (cons (cons 7 (cons 4 '())) '() ))))
)       

;; QUESTION N°2 ;;
(define degreMonome
  (lambda(L)
    (cadr L)))

;; QUESTION N°2 ;;
(define coefficientMonome
  (lambda(L)
    (car L)))

;; QUESTION N°4 ;;
(define degrePolynome
  (lambda(P temp)
    (if (null?  P)
        temp
        (if (<= (degreMonome (car P)) temp)
            (degrePolynome (cdr P) temp)
            (degrePolynome (cdr P) (degreMonome(car P)))))))

(define monomeMin
  (lambda(P M)
    (if (null? P)
        M
        (if(>= (degreMonome (car P)) (degreMonome M))
            (monomeMin (cdr P) M)
            (monomeMin (cdr P) (car P))))))

(define deleteMin
  (lambda(P min T)
    (if (null? P)
        T
        (if(= (degreMonome (car P)) (degreMonome min))
           (deleteMin (cdr P) min T)
           (deleteMin (cdr P) min (cons (car P) T))))))

(define Tri
  (lambda(P)
    (if (null? P)
        '()
        (cons (monomeMin P (car P)) (Tri(deleteMin P (monomeMin P (car P)) '()))))))


;; QUESTION N°5 ;;

(define puissance
  (lambda (x p)
    (if (= p 0)
        1
        (* x (puissance x (- p 1))))))

(define valeurMonome
  (lambda (M v)
    (* (coefficientMonome M) (puissance v (degreMonome M)))))

;; QUESTION N°6 ;;

(define valeurPolynome
  (lambda (P v)
    (if (null? (cdr P))
        (valeurMonome (car P) v)
        (+ (valeurMonome (car P) v) (valeurPolynome (cdr P) v)))))

;; QUESTION N°7 ;;

(define addMonome
  (lambda (M1 M2)
    (cons (+ (coefficientMonome M1) (coefficientMonome M2)) (cons (degreMonome M1) '()))))

(define addPolynome
  (lambda (P1 P2)
    (if (null? P1)
        (if (null? P2)
            '()
            (cons (car P2) (addPolynome P1 (cdr P2))))
        (if (null? P2)
            (cons (car P1) (addPolynome (cdr P1) P2))
            (if (< (degreMonome (car P1)) (degreMonome (car P2)))
                (cons (car P1) (addPolynome (cdr P1) P2))
                (if (> (degreMonome (car P1)) (degreMonome (car P2)))
                    (cons (car P2) (addPolynome P1 (cdr P2)))
                    (cons (addMonome (car P1)(car P2)) (addPolynome (cdr P1)(cdr P2)))))))))

(define vraiAdd
  (lambda (P1 P2)
    (addPolynome (Tri P1)(Tri P2))))

;; QUESTION N°8 ;;

(define derivMonome
  (lambda(M)
    (if (=(degreMonome M) 0)
        0
        (cons (*(degreMonome M) (coefficientMonome M)) (cons (-(degreMonome M) 1) '())))))

(define derivPolynome2
  (lambda(P)
    (if (null? P)
        '()
        (cons (derivMonome (car P)) (derivPolynome2 (cdr P))))))

(define derivPolynome
  (lambda(P)
    (if (= (derivMonome (car P)) 0)
      (derivPolynome2 (cdr P))
      (derivPolynome2 P))))


;; QUESTION N°9 ;;

(define primMonome
  (lambda(M)
    (cons (/ (coefficientMonome M) (+ (degreMonome M) 1) ) (cons (+ (degreMonome M) 1) '()))))

(define primPolynome
  (lambda(P)
    (if (null? P)
        '()
        (cons (primMonome (car P))(primPolynome (cdr P))))))

;; QUESTION N°10 ;;

(define multMonome
  (lambda(M1 M2)
    (list (*(coefficientMonome M1)(coefficientMonome M2)) (+(degreMonome M1)(degreMonome M2)))))

(define place
  (lambda(M P)
    (if (null? P)
        (cons M '())
        (if (< (degreMonome M) (degreMonome(car P)))
            (cons M P)
            (if (= (degreMonome M) (degreMonome(car P)))
                (cons (addMonome M (car P)) (cdr P))
                (cons (car P) (place M (cdr P))))))))

(define produit
  (lambda(M P R)
    (if (null? P)
        R
        (produit M (cdr P) (place (multMonome M (car P)) R)))))

(define produitPolynome
  (lambda(P1 P2)
    (if (null? P1)
        '()
         (addPolynome (produit (car P1) P2 '()) (produitPolynome (cdr P1) P2)))))
            