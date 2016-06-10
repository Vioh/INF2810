;;; syhd - UiO
;;; INF2810 - Vår 2016
;;; Innlevering 1a

;;; -------------- Oppgave (1a) --------------

; Schemes standard evalueringsregel skal brukes for å evaluere vanlige 
; prosedyrer. Reglen sier at vi må først evaluere alle argumentene, og
; så anvendes operatoren (som står først i listen)
; 
; Fordi både * og + er vanlige prosedyrer i Scheme, så kan vi bruke standard
; evalueringsreglen for oppgave 1a:
; 	 ?  (* (+ 4 2) 5)
; 	--> (* 6 5)
; 	--> 30

;;; -------------- Oppgave (1b) --------------
; Prosedyren (* (+ 4 2) (5)) skal gi kjøretidsfeil. 
; I Scheme kan vi bruke parenteser for å spesifisere en prodyre/liste. Og hver
; liste må bestå av et prosedyrenavn (dvs. en operator) som ofte står først i
; lista. Dette betyr at (5) må tolkes som en prosedyre, hvor 5 er operatoren.
; Men evalueringsreglen sier at vi må evaluere 5 som et heltall, ikke som et
; prosedyrenavn! 

;;; -------------- Oppgave (1c) --------------
; Prosedyren (* (4 + 2) 5) også gir kjøretidsfeil. 
; Scheme bruker prefiks notasjon slik at første element i en liste tolkes som
; en operator/prosedyrenavn. Men i listen (4 + 2) som bruker infiksnotasjon, 
; evalueres 4 som et heltall, ikke en operator. For å rette feilen, må vi 
; omskrive (4 + 2) vha. prefiks notasjon slik at dette blir (+ 4 2)

;;; -------------- Oppgave (1d) --------------
; Fordi define er en special form, kan vi ikke bruke standard evalueringsreglen
; for denne oppgaven. I Scheme er define en operasjon som binder en leksikalsk
; variabel til en verdi. Da kan vi tolke (define bar (/ 42 2)) som en måte for
; å binde verdien til prosedyren (/ 42 2) (som evalueres til 21) til en ny
; variabel som heter 'bar'. Hele uttrykket evalueres da til 21.

;;; -------------- Oppgave (1e) --------------
; Fordi 'bar' evalueres til 21 i oppgave 1d, så har vi:
; 	 ?  (- bar 11)
; 	--> (- 21 11)
; 	--> 10

;;; -------------- Oppgave (1b) --------------
;  ?  (/ (* bar 3 4 1) bar)
; --> (/ (* 21 3 4 1) bar)
; --> (/ 252 bar)
; --> (/ 252 21)
; --> 12

;;; -------------- Oppgave (2a) --------------

; For uttrykkene med 'or' eller 'and', evalueres argumentene én etter én fra
; venstre til høyre.
; -  or: hvis et argument evalueres til en sann verdi returneres denne verdien,
;        og ingen flere uttrykk evalueres. Ellers returneres verdien til det
;        siste argumentet. 
; - and: hvis et argument evalueres til en usann verdi returneres #f, og ingen
;        flere uttrykk evalueres. Ellers returneres verdien til det siste
;        argumentet. 
; Dette betyr at for 'or' eller 'and', avhengig av hvilket uttrykk vi får,
; trenger vi ikke nødvendigvis evaluere ALLE argumenter. Fordi Schemes standard
; evalueringsreglen sier at vi må evaluere ALLE argumenter før vi kan bruke
; operatoren, da viser det at 'or' og 'and' er special forms.
; 
; Man kan gi samme begrunnelse for å vise hvorfor 'if' er også en special form.
; Syntaksen for et if-uttrykk består av 3 argumenter:
; 	(if test-predikat then-uttrykk else-uttrykk)
; Hvis test-predikat evalueres til en sann verdi, da trenger man ikke evaluere
; else-uttrykket. Derfor er 'if' en special form.
;
; or-uttrykket i oppgaven skal da evalueres til "piff!" fordi dette er første
; argumentet som gir sann. Syntaktisk feil på det siste argumentet med prefiks
; notasjon (1 - 1) spiller ingen roll fordi vi ikke trenger å evaluere dette
; argumentet.
; 	 ?  (or (= 1 2) "piff!" "paff" (zero? (1 - 1)))
; 	--> piff!
;
; and-uttrykket i oppgaven evalueres til #f fordi (= 1 2) er usann. Dette også
; betyr at vi ikke trenger å evaluere det siste argumentet som har syntaktisk
; feil. Så skal uttrykket ikke gi noen kjøretidsfeil.
; 	 ?  (and (= 1 2) "piff!" "paff!" (zero? (1 - 1)))
; 	--> #f
;
; if-uttrykket i oppgaven evalueres til "poff!" fordi 42 er et positivt tall.
; Selv om siste uttrykket inneholder en prosedyre som ikke er definert, er det
; OK fordi prosedyren ikke trenger evalueres.
; 	 ?  (if (positive? 42) "poff!" (i-am-undefined))
; 	--> poff!

;;; -------------- Oppgave (2b) --------------

; Versjon 1
(define (sign1 x)
  (if (= x 0)
      0
      (if (negative? x); nested "if"
          -1
          1)))

; Version 2
(define (sign2 x)
  (cond ((< x 0) -1); vi kan også bruke "negative?"
        ((> x 0) 1) ; og "positive?" her 
        (else 0)))

;;; -------------- Oppgave (2c) --------------
(define (sign x)
  (or (and (< x 0) -1)
      (and (> x 0) 1)
      (and (= x 0) 0)))
	 
;;; -------------- Oppgave (3a) --------------
(define (add1 x)
  (+ x 1))
(define (sub1 x)
  (- x 1))

;;; -------------- Oppgave (3b) --------------
(define (plus-3b num1 num2)
  (define (plus-iter x y)
    (cond ((zero? y) x)
          ((positive? y) (plus-iter (add1 x) (sub1 y)))
          ((negative? y) (plus-iter (sub1 x) (add1 y)))))
  (plus-iter num1 num2)); basistilfelle
; Slik skal prosedyren fungere: Et av tallene øker med 1, mens det andre tallet
; senker med 1. Dette prosedyren gjentas rekursivt helt til når y=0

;;; -------------- Oppgave (3c) --------------

; Prosedyren definert i oppgave 3b er en rekursiv prosedyre som gir en iterativ
; prosess. Dette er fordi minnebruken ikke vokser. I hele prosessen bruker vi
; bare 2 variabler x og y, som holder tilstanden til prosessen ved ethvert
; tidspunkt. Eksempel på kjøring:
; 	? plus-3b(2, 3)
; 	= plus-iter(2, 3)
; 	= plus-iter(3, 2)
; 	= plus-iter(4, 1)
; 	= plus-iter(5, 0)
; 	= 5

(define (plus-3c num1 num2)
  (define (plus-rek x)
    (cond ((zero? x) num1); basistilfelle
          ((> x 0) (add1 (plus-rek (sub1 x))))
          ((< x 0) (sub1 (plus-rek (add1 x))))))
  (plus-rek num2))
; Her er rekursive prosedyre som gir en rekursiv prosess. Minnebruken for 
; prosessen vokser med tiden, helt til når vi når basistilfellet. Hver gang
; prosedyren kalles, må den vente til når neste prosedyre-kall er ferdig. 
; Dette betyr at prosessen utvikler seg rekursivt. Eksempel på kjøring:
; 	? plus-3c(1, 3)
; 	= plus-rek(3)
; 	= add1(plus-rek(2))
; 	= add1(add1(plus-rek(1)))
; 	= add1(add1(add1(plus-rek(0))))
; 	= add1(add1(add1(2)))
; 	= add1(add1(3))
; 	= add1(4)
; 	= 5

;;; -------------- Oppgave (3d) --------------
(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (+ 1 e))))
  (power-iter 1))
; Vi trenger ikke å ha 'b' og 'n' som parametrene til power-iter fordi de er
; lokale variabler til power-close-to. Dette betyr at 'b' og 'n' kan brukes i
; hele kroppen til power-close-to prosedyren. Siden power-iter er definert
; i power-close-to prosedyren vha. blokkstruktur, så kan power-iter bruke 'b'
; og 'n' selv om de to variablene ikke er med i definisjonen til power-iter.

;;; -------------- Oppgave (3e) --------------
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
; Det er ikke mulig å forenkle fib-iter fordi prosedyren trenger både 'a', 'b',
; og 'count' som parametere, slik at verdiene til de variablene endrer seg når
; prosessen endrer tilstanden. 'a' og 'b' fungerer som akkumulatorvariabler, og
; 'count' er en tellervariabel. Og fordi 'a', 'b', og 'count' ikke er bundne
; til fib, må vi spesifisere de variablene som parametere til fib-iter slik at
; fib-iter kan bruke dem