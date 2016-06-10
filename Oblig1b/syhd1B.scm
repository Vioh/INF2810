;;; syhd - UiO
;;; INF2810 - Vår 2016
;;; Innlevering 1b

;;; -------------- Oppgave (1a-1e) --------------
;(1A):
; 	 ___________
; 	|     |     |
; 	|  o  |  o--|----> 11
; 	|__|__|_____|
; 	   |
; 	   |
; 	   47
;
;(1B):
; 	 _________
; 	|     |  /|
; 	|  o  | / |
; 	|__|__|/__|
; 	   |
; 	   |
; 	   47
;
;(1C):
; 	 ___________       _________
; 	|     |     |     |     |  /|
; 	|  o  |  o--|---->|  o  | / |
; 	|__|__|_____|     |__|__|/__|
; 	   |                 |
; 	   |                 |
; 	   47                11
;
;(1D): '(47 (11 12)) er nøyaktig samme som (list 47 (list 11 12))
; 	 ___________       _________
; 	|     |     |     |     |  /|
; 	|  o  |  o--|---->|  o  | / |
; 	|__|__|_____|     |__|__|/__|
; 	   |                 |
; 	   |               __|________       _________ 
; 	   47             |     |     |     |     |  /|
; 	                  |  o  |  o--|---->|  o  | / |
; 	                  |__|__|_____|     |__|__|/__|
; 	                     |                 |       
; 	                     |                 |
; 	                     11                12
;
;(1E): (cons foo foo) evalueres til ((1 2 3) 1 2 3)
;      begge argumentene peker på samme objekt
; 	 ___________
; 	|     |     |
; 	|  o  |  o  |
; 	|__|__|__|__|                   
; 	   |     |     
;	   |   __|________       ___________       _________ 
; 	   |  |     |     |     |     |     |     |     |  /|
; 	   -->|  o  |  o--|---->|  o  |  o--|---->|  o  | / |
; 	      |__|__|_____|     |__|__|_____|     |__|__|/__|       
; 	         |                 |                 |
; 	         |                 |                 |
; 	         1                 2                 3



;;; -------------- Oppgave (1f-1i) --------------

;(1F)
(car (cdr (cdr '(1 2 3 4))))

;(1G)
(car (car (cdr '((1 2) (3 4)))))

;(1H)
(car (car (cdr (cdr '((1) (2) (3) (4))))))

;(1I)
(cons (cons 1 (cons 2 '())) (cons (cons 3 (cons 4 '())) '())) ;bare vha. cons
(list (list 1 2) (list 3 4)) ;bare vha. list


;;; -------------- Oppgave (2a) --------------
(define (length2 items)
  (define (len-iter liste count)
    (if (null? liste)
        count
        (len-iter (cdr liste) (+ count 1))))
  (len-iter items 0))

;;; -------------- Oppgave (2b) --------------
(define (rev-list items)
  (define (iter x y)
    (if (null? x)
        y
        (iter (cdr x) (cons (car x) y))))
  (iter items '())) ;;basistilfelle

; Prosedyren ovenfor bruker halerekursjon, fordi den starter fra basistilfelle,
; og det siste rekursive kall gir svaret til problemet (dvs. reversert lista).
; Jeg har valgt halerekursjon fordi den er mer naturlig og intuitiv. Hale-
; rekursjon kaller på det første element i lista først, prosessere dette, og
; så forsetter med de andre elementene. Vanlig rekursjon er helt motsatt. Den
; også kaller på det første element først, men må vente på alle de andre
; elementene før den kan prosessere det første elementet. Fordi vi ønsker å
; bygge opp den reversert lista (vha. cons) helt fra bunnen (dvs. fra den tomme
; liste) så er det nødvendig å prosessere første element først, slik at dette
; element går sist i den reversert lista. Derfor er det bedre å bruke hale-
; rekursjon i denne oppgaven.
  
;;; -------------- Oppgave (2c) --------------

(define (ditch x items)
  (cond ((null? items) '())
        ((equal? (car items) x) (ditch x (cdr items)))
        (else (cons (car items) (ditch x (cdr items))))))

; Prosedyren ovenfor bruker vanlig rekursjon fordi siste rekursiv kall gir den
; tomme liste '(), og ikke svaret til prosedyren. 
; Prosessen har kompleksitet O(n), dvs. ressursbehov vokser lineært med input 
; størrelse. Dette er fordi vi må prosessere hvert element i lista en etter en,
; og vi trenger samme ressurs for hvert element. 

;;; -------------- Oppgave (2d) --------------
(define (nth index items)
  (if (zero? index)
      (car items)
      (nth (- index 1) (cdr items))))
	  
;;; -------------- Oppgave (2e) --------------
(define (where x items)
  (define (iter liste index)
    (cond ((null? liste) #f)
          ((equal? (car liste) x) index)
          (else (iter (cdr liste) (+ index 1)))))
  (iter items 0))

;;; -------------- Oppgave (2f) --------------
(define (map2 proc list1 list2)
  (if (or (null? list1) (null? list2))
      '()
      (cons (proc (car list1) (car list2))
            (map2 proc (cdr list1) (cdr list2)))))

;;; -------------- Oppgave (2g) --------------

;gjennomsnittet av 2 tall
(map2 (lambda (x y) (/ (+ x y) 2))
      '(1 2 3 4)
      '(3 4 5))

;sjekke om begge er partall
(map2 (lambda (x y) (and (even? x) (even? y)))
      '(1 2 3 4)
      '(3 4 5))

;;; -------------- Oppgave (2h) --------------
(define (both? pred)
  (lambda (arg1 arg2)
    (and (pred arg1) (pred arg2))))

;;; -------------- Oppgave (2i) --------------
(define (self proc)
  (lambda (x) (proc x x)))