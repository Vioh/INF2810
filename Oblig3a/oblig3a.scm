;;; syhd, emcuevas, hkhermos
;;; INF2810 - Vår 2016
;;; Innlevering 3a
(load "prekode3a.scm")

;;; ------------------------- Oppgave (1a og 1b) ------------------------------
(define proc-table (make-table))

(define (mem message proc)
  (define cache (make-table))
  (define (memoize)
    (lambda args
      (or (lookup args cache)
          (let ((key args)
                (value (apply proc args)))
            (insert! key value cache)
            value))))
  (define (unmemoize)
    (or (lookup proc proc-table)
        proc))                  
  (cond ((equal? message 'memoize) 
         (let ((old-proc proc)
               (new-proc (memoize)))
           (insert! new-proc old-proc proc-table)
           new-proc))
        ((equal? message 'unmemoize) (unmemoize))))

;;test1
(newline) (display "----------- Test1 for (1a og 1b) -----------") (newline)
(set! fib (mem 'memoize fib))
(fib 3) ;new-fib (resultat = 2)
(fib 3) ;new-fib (resultat = 2)
(fib 2) ;new-fib (resultat = 1)
(fib 4) ;new-fib (resultat = 3)
(set! fib (mem 'unmemoize fib))
(fib 3) ;old-fib (resultat = 2)

;;test2
(newline) (display "----------- Test2 for (1a og 1b) -----------") (newline)
(set! test-proc (mem 'memoize test-proc))
(test-proc)                ; 0
(test-proc)                ; 0
(test-proc 40 41 42 43 44) ; 10
(test-proc 40 41 42 43 44) ; 10
(test-proc 42 43 44)       ; 5


;;; ---------------------------- Oppgave (1c) ---------------------------------
; Grunnen at det oppfører ikke seg like som den i 1a og 1b er at symbolet 'mem-fib
; peker på den nye memoisert versjonen av fib, og denne versjonen er en "rekursiv"
; prosedyre som kaller på seg selv ved å bruke symbolet 'fib. Men problemet er at
; symbolet 'fib forsatt peker på den originale prosedyren i prekoden (fordi vi har
; ikke brukt set! til å endre 'fib slik at den peker til den nye versjonen). Det
; betyr at når vi anvender prosedyren mem-fib, så er det bare det første rekursive
; kallet som blir utført som den nye memoiserte versjonen av fib, og resten av
; kallene blir utført som den originale versjonen i prekoden. Dette her leder til
; to ting:
; - Det er bare det første resultatet som blir lagret i cachen, men ikke de andre
;   resultatene. Så for eksempel, hvis vi kaller på (mem-fib 3), så er det bare
;   FIB(3) som blir lagret i cachen, men ikke FIB(2), FIB(1), eller FIB(0).
; - Prosedyren lookup (som søker elementene i cachen) utføres bare i det første
;   rekursive kallet.


;;; ---------------------------- Oppgave (1d) ---------------------------------

; Hjelpeprosedyre som lagrer (save) navngitte variabler og deres tilsvarende verdier
; 'save' prosedyren tar imot en liste av argumenter som spesifiserer default-verdier
(define (save . args)
  (define cache (make-table))
  (define (update! args)
    (if (not (null? args))
        (let ((k (car args))
              (v (cadr args)))
          (insert! k v cache)
          (update! (cddr args)))))
  (define (search key)
    (or (lookup key cache)
        "\"Error: Key not found\""))
  (define (dispatch message . args)
    (cond ((eq? message 'update!) (update! args))
          ((eq? message 'search) (search (car args)))))
  (update! args) ;lagre default-verdier
  dispatch)

(define (greet . args)
  (let ((cache (save 'title "friend" 'time "day")))
    (apply cache 'update! args)
    (display "good ")
    (display (cache 'search 'time)) (display " ")
    (display (cache 'search 'title)) (display " ")
    (newline)))

;;test
(newline) (display "-------------- Test for (1d) --------------") (newline)
(greet)                                 ;good day friend
(greet 'time "evening")                 ;good evening friend
(greet 'title "sir" 'time "morning")    ;good morning sir
(greet 'time "afternoon" 'title "dear") ;good afternoon dear


;;; ---------------------------- Oppgave (2a) ---------------------------------
(define (list-to-stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst) (list-to-stream (cdr lst)))))

(define (stream-to-list stream . args)
  (define (iter stream num)
    (if (or (stream-null? stream) (= num 0))
        '()
        (cons (car stream) (iter (stream-cdr stream) (- num 1)))))
  (if (null? args)
      (iter stream -1)
      (iter stream (car args))))

;;test
(newline) (display "-------------- Test for (2a) --------------") (newline)
(list-to-stream '(1 2 3 4 5))            ;(1 . #<promise>)
(stream-to-list (stream-interval 10 20)) ;(10 11 12 13 14 15 16 17 18 19 20)
(show-stream nats 15)                    ;1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...
(stream-to-list nats 10)                 ;(1 2 3 4 5 6 7 8 9 10)


;;; ---------------------------- Oppgave (2b) ---------------------------------
(define (stream-map proc . argstreams)
  (define (done? argstreams)
    (cond ((null? argstreams) #f)
          ((stream-null? (car argstreams)) #t)
          (else (done? (cdr argstreams)))))
  (if (or (null? argstreams) (done? argstreams))
      the-empty-stream         
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))

;;test
(newline) (display "-------------- Test for (2b) --------------") (newline)
(define squares (stream-map * nats nats))
(show-stream squares 5) ;1 4 9 16 25 ...


;;; ---------------------------- Oppgave (2c) ---------------------------------
; Et potensielt problem er når vi sender en uendelig strøm inn i prosedyren 'memq'. Da skal
; vi aldri få noen resultat fra 'memq', fordi det er umulig å sjekke om et item er et medlem
; av en uendelig strøm.


;;; ---------------------------- Oppgave (2d) ---------------------------------
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (stream-filter (lambda (x) (not (eq? x (stream-car stream))))
                                  (remove-duplicates (stream-cdr stream))))))

; Prosedyren over fungerer veldig fint når vi har en UENDELIG strøm med et UENDELIG antall
; elementer som er forskjellige fra hverandre. Men hvis vi har en UENDELIG strøm med et
; ENDELIG antall elementer som er forskjellige fra hverandre, så terminerer prososedyren
; ikke. Problemet er at når prosedyren har funnet det siste unique elementet, så terminerer
; prosedyren ikke, men forsette med den uendelige strømmen for å finne neste unique elementet
; som faktisk ikke finnes!

;;test
(newline) (display "-------------- Test for (2d) --------------") (newline)
(show-stream (remove-duplicates (list-to-stream '(1 1 2 3 4 3)))) ;1 2 3 4
(show-stream (remove-duplicates nats)) ;1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...
;(define onetwo (cons-stream 1 (cons-stream 2 onetwo))) ;==> onetwo = 1 2 1 2 1 2 ....
;(show-stream (remove-duplicates onetwo)) ;==> returnerer 1 og 2, men terminerer ikke!


;;; ---------------------------- Oppgave (2e) ---------------------------------
(newline) (display "-------------- Test for (2e) --------------") (newline)
(display "Kallet på define:") (newline)
(define x (stream-map show (stream-interval 0 10))) ;0 skrives ut til skjerm via display

; En forskjell mellom en liste og en strøm er NÅR vi henter ut elementene. Hvis vi har en
; liste, så henter vi alle elementene på en gang. Men hvis vi har en strøm isteden, så
; henter vi bare de elementene som vi faktisk trenger. En annen ting er at siden vi har
; brukt stream-map, så må vi bruke prosedyren 'show' hver gang vi henter et nytt element
; ut fra intervallet. 
;
; I kallet over, er det bare det første elementet fra intervallet som ble hentet. Derfor
; er det bare tallet 0 som ble skrevet ut til skjermen via display. I tillegg må vi huske
; at 'show' ikke bare viser et element på skjerm, men den returnerer også elementet ut.
; Elementene som returneres blir da samlet inn i en strøm via stream-map. Denne strømmen
; blir da bundet til symbolet 'x ved kallet på define over.

(display "Første kallet på stream-ref:") (newline)
(stream-ref x 5) ;1->5 skrives ut til skjerm via display. Hele uttrykket evalueres til 5
;                ;fordi 5 er verdien som ligger på indeksen 5 i 'x'

; Ved kallet på stream-ref over, trenger vi å hente ut alle elementene som ligger mellom
; indeksen 0 og 5 i strømmen 'x'. Men for å gjøre dette, må vi først hente ut disse sex
; elementene fra intervallet.
;
; Siden vi har allerede hentet ut verdien 0 ved kallet på define, trenger vi ikke å hente
; den ut igjen (denne verdien har allerede blitt memorisert i interne cachen, siden delay
; er en memoirisert prosedyre). Men dette gjelder ikke for de neste 5 elementer fra
; intervallet, dvs. vi må hente dem ut. Siden prosedyren 'show' brukes hver gang vi henter
; ut et nytt element, derfor er det bare tallene 1->5 som blir skrevet ut til skjermen via
; prosedyren 'show'.

(display "Siste kallet på stream-ref:") (newline)
(stream-ref x 7) ;6 og 7 skrives ut til skjerm via display. Hele uttrykket evalueres til 7
;                ;fordi 7 er verdien som ligger på indeksen 7 i 'x'

; På samme måte, her trenger vi alle elementene mellom indeksen 0 og 7 i 'x'. Da trenger
; vi disse 8 elementene fra intervallet. Men siden vi har allerede memorisert de verdiene
; fra indeksen 0 til 5, så trenger vi bare å hente ut de verdiene som ligger på indeksen
; 6 og 7. Derfor er det bare 6 og 7 som ble skrevet ut til skjem via display ved kallet over.


;;; ---------------------------- Oppgave (2f) ---------------------------------
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams nats factorials)))

;;test
(newline) (display "-------------- Test for (2f) --------------") (newline)
(stream-ref factorials 0) ;1
(stream-ref factorials 1) ;1
(stream-ref factorials 5) ;120