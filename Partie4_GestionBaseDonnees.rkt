#lang scheme

(define creationAdresse
  (lambda(num rue cp ville pays)
    (list num rue cp ville pays)))

(define creationTelephone
  (lambda(x)
    (list x)))

(define creationId
  (lambda(id nom prenom date)
    (list id nom prenom date)))

(define creationPersonne
  (lambda(id tel adresse)
    (list id tel adresse)))

(define listePersonne
  (cons (creationPersonne (creationId 0 'Ex 'Ex '23oct) (creationTelephone 3) (creationAdresse 1 'a 60000 'b 'c))'()))

(define listePersonne2
  (cons (creationPersonne (creationId 2 'P 'P1 '7fev) (creationTelephone 5) (creationAdresse 3 'r 60000 'b 'c)) listePersonne)) 

(define listePersonne3
  (cons (creationPersonne (creationId 3 'P 'P1 '14dec) (creationTelephone 6) (creationAdresse 7 'p 59300 'Val 'c)) listePersonne2))
  
(define P1
  (creationPersonne (creationId 0 'Ex2 'Ex2 '1janv)(creationTelephone 4)(creationAdresse 2 'rue 59300 'Val 'France)))

(define P2
  (creationPersonne (creationId 1 'Ziane 'Sofiane '15juin)(creationTelephone 0671)(creationAdresse 16 'Porchelets 59300 'Valenciennes 'France)))

(define presenceId?
  (lambda(id L)
    (if (null? L)
        #f
        (if (= id (car(caar L)))
           #t
           (presenceId? id (cdr L))))))

(define ajoutPersonne
  (lambda(personne L)
    (if (presenceId? (caar personne) L)
        L
        (cons personne L))))

(define detruirePersonne
  (lambda(id L)
    (if (null? L)
        '()
        (if (= id (car(caar L)))
            (detruirePersonne id (cdr L))
            (cons (car L) (detruirePersonne id (cdr L)))))))


(define rechercheId
  (lambda(id L)
    (if (null? L)
        '()
        (if (= id (car(caar L)))
           (car L)
           (rechercheId id (cdr L))))))

(define rechercheNom
  (lambda(nom L)
    (if (null? L)
        '()
        (if (eq? nom (car(cdr(car(car L)))))
            (cons (car L) (rechercheNom nom (cdr L)))
            (rechercheNom nom (cdr L))))))

(define recherchePrenom
  (lambda(prenom L)
    (if (null? L)
        '()
        (if (eq? prenom (car(cdr(cdr(car(car L))))))
            (cons (car L) (recherchePrenom prenom (cdr L)))
            (recherchePrenom prenom (cdr L))))))

(define rechercheTel
  (lambda(tel L)
    (if (null? L)
        '()
        (if (= tel (car(car(cdr(car L)))))
            (cons (car L) (rechercheTel tel (cdr L)))
            (rechercheTel tel (cdr L))))))

(define comparaison
  (lambda(L1 L2)
    (if (and (= (car L1)(car L2))
             (eq? (car(cdr L1))(car(cdr L2)))
             (= (car(cdr(cdr L1)))(car(cdr(cdr L2))))
             (eq? (car(cdr(cdr(cdr L1))))(car(cdr(cdr(cdr L2)))))
             (eq? (car(cdr(cdr(cdr(cdr L1)))))(car(cdr(cdr(cdr(cdr L1))))))
         )
        #t
        #f)))

(define rechercheAdresse
  (lambda(ad L)
    (if (null? L)
        '()
        (if (comparaison ad (car(cdr(cdr(car L)))))
            (cons (car L) (rechercheAdresse ad (cdr L)))
            (rechercheAdresse ad (cdr L))))))

(define getPersonne
  (lambda(maBase champ val)
    (if (eq? champ 'id)
        (rechercheId val maBase)
        (if (eq? champ 'nom)
            (rechercheNom val maBase)
            (if (eq? champ 'prenom)
                (recherchePrenom val maBase)
                (if (eq? champ 'tel)
                    (rechercheTel val maBase)
                    (if (eq? champ 'adresse)
                        (rechercheAdresse val maBase)
                        'RechercheImpossible)))))))
    
  