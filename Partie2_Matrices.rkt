#lang scheme

(define m3x3
  (lambda(a11 a12 a13 a21 a22 a23 a31 a32 a33)
    (cons (cons a11 (cons a12 (cons a13 '()))) (cons (cons a21 (cons a22 (cons a23 '()))) (cons (cons a31 (cons a32 (cons a33 '()))) '())))))

(define t1
  (lambda(m3x3)
    (if (null? m3x3)
        '()
        (cons (car(car m3x3)) (t1 (cdr m3x3))))))

(define t2
  (lambda(m3x3)
    (if (null? m3x3)
        '()
        (cons (car(cdr(car m3x3))) (t2 (cdr m3x3))))))

(define t3
  (lambda(m3x3)
    (if (null? m3x3)
        '()
        (cons (car(cdr(cdr(car m3x3)))) (t3 (cdr m3x3))))))

(define trans
  (lambda(m3x3)
    (cons (t1 m3x3) (cons (t2 m3x3) (cons (t3 m3x3) '())))))


(define estSymetrique
  (lambda(m3x3)
    (if (= (car(cdr(car m3x3))) (car(car(cdr m3x3))))
        (if (= (car(cdr(cdr(car m3x3)))) (car(car(cdr(cdr m3x3)))))
            (if (= (car(cdr(car(cdr(cdr m3x3))))) (car(cdr(cdr(car(cdr m3x3))))))
                #t
                #f)
            #f)
        #f)))


(define sommeL
  (lambda(a b)
    (if (null? a)
             '()
             (cons (+ (car a)(car b)) (sommeL (cdr a)(cdr b))))))

(define somme
  (lambda(A B)
    (if (null? A)
        '()
        (cons (sommeL (car A) (car B)) (somme (cdr A) (cdr B))))))


(define prodLigne
  (lambda(L1 L2)
    (if (null? L1)
        0
        (+ (*(car L1)(car L2)) (prodLigne (cdr L1)(cdr L2))))))

(define preprod
  (lambda(L B)
    (if (null? B)
        '()
        (cons (prodLigne L (car B)) (preprod L (cdr B))))))

(define produit
  (lambda(A B)
    (if (null? A)
        '()
        (cons (preprod (car A)(trans B)) (produit (cdr A) B)))))


    