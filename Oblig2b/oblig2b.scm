;;; syhd, emcuevas, hkhermos
;;; INF2810 - Vår 2016
;;; Innlevering 2b

;;; ---------------------------- Oppgave (1a) ---------------------------------
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

;;test
(display "Test for 1a: ") (newline)
(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))
(c1)  ;;1
(c1)  ;;2
(c1)  ;;3
count ;;42
(c2)  ;;1

;;; ---------------------------- Oppgave (1b) ---------------------------------
; Se PDF-fil

;;; ---------------------------- Oppgave (2a) ---------------------------------
(define (make-stack stk)
  (define (push! items)
    (if (not (null? items))
        (begin
          (set! stk (cons (car items) stk))
          (push! (cdr items)))))
  (define (pop!)
    (if (not (null? stk))
        (set! stk (cdr stk))))
  (define (dispatch message . args)
    (cond ((eq? message 'push!) (push! args))
          ((eq? message 'pop!) (pop!))
          ((eq? message 'stack) stk)))
  dispatch)
  
;;test
(newline) (display "Test for 2a: ") (newline)
(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack)  ;;(bar)
(s2 'pop!)
(s2 'push! 1 2 3 4)
(s2 'stack)  ;;(4 3 2 1)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack)  ;;(baz zip zap bah bar)

;;; ---------------------------- Oppgave (2b) ---------------------------------
(define (pop! stk)
  (stk 'pop!))
(define (push! stk . items)
  (apply stk 'push! items))
(define (stack stk)
  (stk 'stack))

;;test
(newline) (display "Test for 2b: ") (newline)
(pop! s1)
(stack s1) ;(zip zap bah bar)
(push! s1 'foo 'faa)
(stack s1) ;(faa foo zip zap bah bar)

;;; ---------------------------- Oppgave (3a) ---------------------------------
; Etter kallet (define bar (list 'a 'b 'c 'd 'e)):
;         __________     __________     __________     __________     _________
;        |     |    |   |     |    |   |     |    |   |     |    |   |     |  /|
; bar -->|  o  |  --|-->|  o  |  --|-->|  o  |  --|-->|  o  |  --|-->|  o  | / |
;        |__|__|____|   |__|__|____|   |__|__|____|   |__|__|____|   |__|__|/__|
;           |              |              |              |              |
;          'a             'b             'c             'd             'e
; 
; Etter kallet (set-cdr! (cdddr bar) (cdr bar)):
;                           ___________________________________
;                          |                                   |
;         __________     __V_______     __________     ________|_    
;        |     |    |   |     |    |   |     |    |   |     |  | |   
; bar -->|  o  |  --|-->|  o  |  --|-->|  o  |  --|-->|  o  |  o |
;        |__|__|____|   |__|__|____|   |__|__|____|   |__|__|____|   
;           |              |              |              |              
;          'a             'b             'c             'd             
; 
; list-ref returnerer et element i en gitt posisjon i en liste. Etter kallet på
; set-cdr! over, så peker bar til en sirkulær liste (som egentlig er en uendelig
; liste), hvor den siste cons-cellen peker tilbake til den andre cons-cellen.
; Med andre ord, listen ser ut slik: (a b c d b c d ...), med b, c, og d gjentas
; i en uendelig løkke. Det betyr at:
; - Hvis p = 3k-2, så får vi 'b (hvor p er posisjon, og k er en positiv integer)
; - Hvis p = 3k-1, så får vi 'c
; - Hvis p = 3k  , så får vi 'd


;;; ---------------------------- Oppgave (3b) ---------------------------------
; Etter kallet (define bah (list 'bring 'a 'towel)):
;         __________     __________     _________
;        |     |    |   |     |    |   |     |  /|
; bah -->|  o  |  --|-->|  o  |  --|-->|  o  | / |
;        |__|__|____|   |__|__|____|   |__|__|/__|
;           |              |              |
;        'bring           'a           'towel
;
; Etter kallet (set-car! bah (cdr bah)):
;            ______________
;           |              |
;         __|_______     __V_______     _________
;        |  |  |    |   |     |    |   |     |  /|
; bah -->|  o  |  --|-->|  o  |  --|-->|  o  | / |
;        |_____|____|   |__|__|____|   |__|__|/__|
;                          |              |
;                         'a           'towel
;
; Nå kaller vi på set-car! igjen:
; 	(set-car! (car bah) 42)
; Her kan vi tolke (car bah) som en peker til den andre cons-cellen (som holder 
; symbolet 'a). Og hva set-car! gjør er å endre første delen av denne cons-cellen 
; slik at den skal peke til et objekt som representerer tallet 42. Men fordi både 
; (car bah) og (cdr bah) peker til samme cons-cellen (som nå holder verdien 42
; istedet for symbolet 'a), så vil både car og cdr til bar inneholde 42, og ikke
; symbolet 'a.
; Slik skal diagrammet se ut etter siste kallet på set-car!:
;            ______________
;           |              |
;         __|_______     __V_______     _________
;        |  |  |    |   |     |    |   |     |  /|
; bah -->|  o  |  --|-->|  o  |  --|-->|  o  | / |
;        |_____|____|   |__|__|____|   |__|__|/__|
;                          |              |
;                         42           'towel


;;; ---------------------------- Oppgave (3c) ---------------------------------
(define (cycle? lst)
  (not (list? lst)))

; Implementasjonen ovenfor er en mulig løsning til oppgaven. Men fordi den bruker
; list? prosedyre, som er innebygd i Scheme, så skrev vi også to andre alternative
; løsninger til oppgaven:

(define (member? obj lst)
  (cond ((null? lst) #f)
        ((eq? obj (car lst)) #t)
        (else (member? obj (cdr lst)))))
(define (cycle? lst)
  (define (iter unvisited visited)
    (cond ((null? unvisited) #f)
          ((member? unvisited visited) #t)
          (else (iter (cdr unvisited)
                      (cons unvisited visited)))))
  (iter lst '()))

; Algoritmen over kan beskrives slik: Hver cons-celle kan enten være besøkt 
; (visited) eller ikke besøkt (unvisited). Og hver gang vi besøker en cons-
; celle, så sjekker vi om denne cons-cellen har allerede vært besøkt før:
; - Hvis ja, så finnes det en cycle i listen
; - Hvis ikke, så markerer vi denne cons-cellen som "besøkt", og så forsetter
;   vi med algoritmen.
; Prosedyren som beskrives fungerer men er lite effektiv fordi den har lineær
; minnebruk (O(n)) og kvadratisk tidskompleksitet (O(n^2)).
; Minnebruk er lineær fordi vi trenger plass til å lagre alle cons-celler som
; vi har besøkt, og jo større listen er, jo flere cons-celler som vi må besøke
; Tidsbruk er kvadratisk fordi for hver cons-celle, sjekker vi om den allerede
; er besøkt ved å sammenligne den med alle de cons-cellene som vi har besøkt.

(define (has-cycle? lst)
  (define (iter slow fast)
    (cond ((or (null? fast) (null? (cdr fast))) #f)
          ((eq? slow (cdr fast)) #t)
          (else (iter (cdr slow) (cddr fast)))))
  (iter lst lst))
  
; has-cycle? prosedyren over er en implementasjon av tortoise-hare algoritmen.
; Vi skrev denne prosedyren basert på et spørsmål på StackOverflow:
; http://stackoverflow.com/questions/29252994/procedure-to-check-if-list-is-cyclic-scheme
; 
; Algoritmen spesifiserer 2 pekere, en som går fast, og en som går slow, og
; begge to vil starte fra den første cons-cellen. Hvis "fast" tar igjen "slow", 
; så finnes det en cycle i lista.
; 
; Algoritmen har konstant minnebruk fordi alt vi trenger er bare de to pekere.
; Tidskompleksiteten er lineær fordi "fast" vil alltid ta igjen "slow" etter
; "fast" har gått 2 løkker gjennom listen. 
  
;;test
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))
(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
(set-car! (car bah) 42)
(newline) (display "Test for 3c (cycle?)") (newline)
(cycle? '(hey ho))   ;;#f
(cycle? '(la la la)) ;;#f
(cycle? bah)         ;;#f
(cycle? bar)         ;;#t
(newline) (display "Test for 3c (has-cycle?)") (newline)
(has-cycle? '(hey ho))   ;;#f
(has-cycle? '(la la la)) ;;#f
(has-cycle? bah)         ;;#f
(has-cycle? bar)         ;;#t  
  
  
;;; ---------------------------- Oppgave (3d) ---------------------------------
; En liste i Scheme er definert som et kjede av cons-par der siste elementet er
; den tomme listen '(). Det betyr at list? er en prosedyre som sjekker om cdr til 
; den siste cons-cellen faktisk peker til '() eller ikke. Men siden det er ikke
; noen "siste" cons-celle i en sirkulær liste (fordi elementene i listen gjentas
; i en uendelig løkke), så vil list? returnere false hvis input-argumentet er en
; sirkulær liste (som f.eks. bar)


;;; ---------------------------- Oppgave (3e) ---------------------------------
(define (top ring)
  (ring 'top))

(define (left-rotate! ring)
  (ring 'left-rotate!))

(define (right-rotate! ring)
  (ring 'right-rotate!))

(define (insert! ring item)
  (ring 'insert! item))

(define (delete! ring)
  (ring 'delete!))

(define (copy-list lst)
  (append lst '()))

  
(define (make-ring input)
  (define ring (copy-list input))
  (define (make-ring! pointer)
    (if (null? (cdr pointer))
        (set-cdr! pointer ring) ;;peker tilbake på første cons-cellen
        (make-ring! (cdr pointer))))
  (define (top)
    (if (null? ring)
        '()
        (car ring)))
  (define (left-rotate!)
    (if (not (null? ring))
        (set! ring (cdr ring)))
    (top))
  (define (right-rotate!)
    (define (loop pointer)
      (if (eq? (cdr pointer) ring)
          (set! ring pointer)
          (loop (cdr pointer))))
    (if (not (null? ring))
        (loop ring))
    (top))
  (define (insert! item)
    (cond ((null? ring) (set! ring (list item))
                        (set-cdr! ring ring) ;;peker på seg selv
                        (top))
          (else (right-rotate!)
                (set-cdr! ring (cons item (cdr ring)))
                (left-rotate!))))
  (define (delete!)
    (cond ((or (null? ring) (eq? (cdr ring) ring)) (set! ring '()) (top))
          (else (right-rotate!)
                (set-cdr! ring (cddr ring))
                (left-rotate!))))
  (define (dispatch message . item)
    (cond ((eq? message 'top) (top))
          ((eq? message 'left-rotate!) (left-rotate!))
          ((eq? message 'right-rotate!) (right-rotate!))
          ((eq? message 'insert!) (insert! (car item)))
          ((eq? message 'delete!) (delete!))))
  (if (not (null? input)) 
      (make-ring! ring))
  dispatch)

		 
;;test
(newline) (display "Test for 3e: ") (newline)
(define r1 (make-ring '(1 2 3 4)))
(define r2 (make-ring '(a b c d)))
(top r1) ;;1
(top r2) ;;a
(right-rotate! r1) ;;4
(left-rotate! r1)  ;;1
(left-rotate! r1)  ;;2
(delete! r1)       ;;3
(left-rotate! r1)  ;;4
(left-rotate! r1)  ;;1
(left-rotate! r1)  ;;3
(insert! r2 'x)    ;;x
(right-rotate! r2) ;;d
(left-rotate! r2)  ;;x
(left-rotate! r2)  ;;a
(top r1)           ;;3



;;; ---------------------------- Oppgave (3f) ---------------------------------
; left-rotate! behøver ikke å traversere ringen, så har denne prosedyren O(1)
; tidskompleksitet (dvs. konstant tidsbruk) 
;
; right-rotate! har en indre "loop" prosedyre som traversere hele ringen før
; den kan gjøre endring til data-strukturen. Derfor er tidskompleksiteten O(n)
;
; insert! bruker både right-rotate! og left-rotate!. Dette gir O(n)+O(1) = O(n)
; tidskompleksitet
; 
; delete! bruker også right-rotate! og left-rotate!, så får vi samme kompleksitet
; for delete! (nemlig O(n), eller lineær tidsbruk)
;
; Merk at i analysen over, tar vi bare hensyn til verste tilfellene. 