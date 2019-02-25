#lang scheme

(define f
  (lambda(x)
    (* 2 x)))

;; Fonction que l'on cherche a intégré ;;

(define calculMk
  (lambda(k n a b)
     (+ a (- k 1) ( / (- b a) n) (/ ( / (- b a) n) 2))))

;; Calcul de la valeur de Mk , soit le point de milieu de segment ;;

(define sommeFMk
  (lambda(f k n a b)
    (if (= k n)
        0
        (+ (f (calculMk k n a b)) (sommeFMk f (+ k 1) n a b)))))

;; Calcul de la somme des f(mk) ;;

(define inte
  (lambda(f n a b)
     (* ( / (- b a) n) (sommeFMk f 1 n a b))))

;; Calcul de l'intégrale ;;