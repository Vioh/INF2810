;;; syhd, emcuevas, hkhermos
;;; INF2810 - Vår 2016
;;; Innlevering 2a
(load "huffman.scm")

;;; -------------- Oppgave (1a) --------------
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car param)
  (param (lambda (arg1 arg2) arg1)))

(define (p-cdr param)
  (param (lambda (arg1 arg2) arg2)))

;;; -------------- Oppgave (1b) --------------
(define foo 30)
((lambda (x y) (+ foo x y)) foo 20)
((lambda (foo) ((lambda (x) ((lambda (y) (+ foo x y)) 20)) foo)) 10)

;;; -------------- Oppgave (1c) --------------
; 	?  (define a1 (list 1 2 3 4))
; 	?  (define a2 (list + - * /))
; 	?  (define a3 (list 5 6 7 8))
; 	?  (map (lambda (x y z) (y x z)) a1 a2 a3)
; 	-> (6 -4 21 1/2)
;
; Det som skjer i kallet over er at MAP bruker en operator i listen a2
; til å gjøre operasjonen på tilsvarende operander i listen a1 og a3. 
; Dette gjentas for alle elementer i listene.
; 
; Eksempel på et kall til anonyme prosedyren uten MAP:
; 	?  (lambda (x y z) (y x z)) 1 + 5)
; 	-> 6
; Dette vil si at vi kan taste inn argumentene i infiks rekkefølge, og 
; hva anonyme prosedyren gjør er å konvertere argumentene til postfiks
; rekkefølge. 
;
;;; -------------- Oppgave (2a) --------------
(define (member? symbol items)
  (cond ((null? items) #f)
        ((eq? symbol (car items)) #t)
        (else (member? symbol (cdr items)))))

;;; -------------- Oppgave (2b) --------------
; decode-1 henter bit for bit og dekoderer den. Hver gang vi finner et symbol 
; må vi gå tilbake til det originale treet (altså roten). Og hvis vi ikke hadde 
; dekode-1 så har vi ikke tilgang til det originale treet siden current-branch'
; endrer seg hele tiden.

;;; -------------- Oppgave (2c) --------------
(define (decode-hale bits tree)
  (define (iter message bits current-branch) ;;message er endelige output til decode-hale
    (if (null? bits) 
        message
        (let ((next-branch 
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (iter (append message (cons (symbol-leaf next-branch) '()))
                    (cdr bits)
                    tree)
              (iter message (cdr bits) next-branch)))))
  (iter '() bits tree))

;;; -------------- Oppgave (2d) --------------
; Resultatet blir:
; 	(ninjas fight ninjas by night)
;
;;; -------------- Oppgave (2e) --------------
(define (encode message tree)
  (define (encode-1 symbol current-branch) ;;encode-1 enkoderer et symbol om gangen
    (if (leaf? current-branch)
        '()
        (let ((left  (left-branch current-branch))
              (right (right-branch current-branch)))
          (cond ((member? symbol (symbols left)) (cons 0 (encode-1 symbol left)))
                ((member? symbol (symbols right)) (cons 1 (encode-1 symbol right)))
                (else '())))))
  (if (null? message)
      '()
      (append (encode-1 (car message) tree)
              (encode   (cdr message) tree))))
			  	  
;;; -------------- Oppgave (2f) --------------  
(define (grow-huffman-tree freqs)
  (define (iter set)      ;;vi antar at 'set' ikke er tom
    (if (null? (cdr set))
        (car set)         ;;ferdig når 'set' har bare 1 element (roten til treet)
        (iter (adjoin-set 
               (make-code-tree (cadr set) (car set)) ;;ny node
               (cddr set)))))
  (iter (make-leaf-set freqs)))
    
;;; -------------- Oppgave (2g) -------------- 
(define mycodebook (grow-huffman-tree
                    '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3)
					  (in 2) (ambush 2) (defeat 1) (the 5) (sword 4)
					  (by 12) (assassin 1) (river 2) (forest 1)
					  (wait 1) (poison 1))))
(encode '(ninjas fight ninjas fight ninjas ninjas fight samurais samurais fight
                 samurais fight ninjas ninjas fight by night)
        mycodebook)
		
; Dette gir (1010110101101101010000010001101101011000111) som resultat. 
; Det er 43 bits alle sammen ==> gjennomsnittlige lengden er 43/17 = 2.5 bits per ord
; Hvis vi har brukt en kode med fast lengde istedet, så trenger vi minst 4 bits per ord.
; Dette er fordi alfabetet har 16 ord, derfor trenger vi 2^4 forskjellige bits-mønstre

;;; -------------- Oppgave (2h) --------------
(define (huffman-leaves tree)
  (if (leaf? tree)
      (cons (cdr tree) '())
      (append (huffman-leaves (left-branch tree))
              (huffman-leaves (right-branch tree)))))

;;; -------------- Oppgave (2i) --------------
(define (expected-code-length tree)
  (let ((total-weight (cadddr tree)))
    (define (traverse branch)
      (if (leaf? branch)
          (* (/ (caddr branch) total-weight)                  ;;sansynligheten 
             (length (encode (cons (cadr branch) '()) tree))) ;;antall bits
          (+ (traverse (left-branch branch))
             (traverse (right-branch branch)))))
    (traverse tree)))
