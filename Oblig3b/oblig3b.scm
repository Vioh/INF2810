;;; syhd, emcuevas, hkhermos
;;; INF2810 - Vår 2016
;;; Innlevering 3b
(load "evaluator.scm")
(set! the-global-environment (setup-environment))

;;; -------------------------- Oppgave (1a) -------------------------------
(display "\n============= Test for (1a) =============\n")
(mc-eval '(define (foo cond else)
            (cond ((= cond 2) 0)
                  (else (else cond)))) the-global-environment) ;Uttrykk 1: ok
(mc-eval '(define cond 3) the-global-environment)              ;Uttrykk 2: ok
(mc-eval '(define (else x) (/ x 2)) the-global-environment)    ;Uttrykk 3: ok
(mc-eval '(define (square x) (* x x)) the-global-environment)  ;Uttrykk 4: ok
(mc-eval '(foo 2 square) the-global-environment)               ;Uttrykk 5: 0
(mc-eval '(foo 4 square) the-global-environment)               ;Uttrykk 6: 16
(mc-eval '(cond ((= cond 2) 0)
                (else (else 4))) the-global-environment)       ;Uttrykk 7: 2

; Måten som meta-evaluatoren bruker til å evaluere et cond-uttrykk er å først
; oversette hele uttrykket til et if-uttrykk (vha. cond->if prosedyren), og
; så evaluere dette if-uttrykket med eval-if. Merk at et if-uttrykk har ikke
; noen nøkkelord som f.eks 'cond eller 'else.
; På grunn av denne derivasjonen fra 'cond' til 'if', så er det helt OK med
; å ha flere forekomster av 'cond og 'else i prosedyren 'foo'. Dette gjelder
; også for uttrykk 7.
;
; Uttrykk 5-6 representerer et vanlig prosedyre kall, som egentlig vil bli
; evaluert av mc-apply. Hva som skjer er da globale omgivelsen blir utvidet 
; (vha. prosedyren extend-environment) med en ny ramme, hvor parametrene til
; 'foo' (dvs. 'cond og 'else) blir bundet til de argumentene 'foo' ble kallet
; på (dvs. 2, 4, og square). Deretter skal vi evaluere 'foo' i denne utvidet
; omgivelsen. Resultat må da være 0 (for uttrykk 5) og 16 (for uttrykk 6).

; Siden uttrykk 7 er en special-form? av typen 'cond', så utvider vi ikke
; den globale omgivelsen når vi evaluerer uttrykket. Dette betyr at 'cond
; og 'else skal bli evaluert til de verdiene som har allerede blitt definert
; i uttrykk 2 og 3. Resultat må da være 2.



;;; -------------------------- Oppgave (2a) -------------------------------
(set! primitive-procedures
      (append (list (list '1+ (lambda (x) (+ x 1)))
                    (list '1- (lambda (x) (- x 1))))
              primitive-procedures))
(set! the-global-environment (setup-environment))



;;; -------------------------- Oppgave (2b) -------------------------------
(define (install-primitive! name proc)  
  (set! primitive-procedures
        (cons (list name proc)
              primitive-procedures))
  (add-binding-to-frame! name
                         (list 'primitive proc)
                         (first-frame the-global-environment)))



;;; -------------------------- Oppgave (3a) -------------------------------
(define (and? exp) (tagged-list? exp 'and))

(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (define (recur args)
    (let ((value (mc-eval (car args) env)))
      (cond ((false? value) #f)
            ((null? (cdr args)) value)
            (else (recur (cdr args))))))
  (if (null? (cdr exp))
      #t
      (recur (cdr exp))))

(define (eval-or exp env)
  (define (recur args)
    (let ((value (mc-eval (car args) env)))
      (cond ((true? value) value)
            ((null? (cdr args)) #f)
            (else (recur (cdr args))))))
  (if (null? (cdr exp))
      #f
      (recur (cdr exp))))



;;; -------------------------- Oppgave (3b) -------------------------------
(define (new-if? exp) (tagged-list? (cddr exp) 'then))

(define (eval-newif exp env)
  (let ((value (mc-eval (if-predicate exp) env)))
    (cond ((tagged-list? exp 'else) value)
          ((true? value) (mc-eval (cadddr exp) env))
          (else (eval-newif (cddddr exp) env)))))

;;eval-if fra prekoden blir utvidet slik:
(define (eval-if exp env)
  (if (new-if? exp)        ;ny
      (eval-newif exp env) ;ny
      (if (true? (mc-eval (if-predicate exp) env))
          (mc-eval (if-consequent exp) env)
          (mc-eval (if-alternative exp) env))))



;;; ------------------------ Oppgave (3c og 3d) ----------------------------
(define (let? exp) (tagged-list? exp 'let))

(define (let3c? exp) (list? (cadr exp)))

(define (eval-let exp env)
  (if (let3c? exp)
      (mc-eval (let3c->lambda exp) env)
      (mc-eval (let3d->lambda exp) env)))

(define (build-lambda-exp vars body args)
  (cons (cons 'lambda (cons vars body)) args))

(define (let3c->lambda exp)
  (let ((vars (map car (cadr exp)))
        (body (cddr exp))
        (args (map cadr (cadr exp))))
    (build-lambda-exp vars body args)))

(define (let3d->lambda exp)
  (define vars '())
  (define body '())
  (define args '())
  (define (recur! rest)         ;recur! skal endre vars, body, og args
    (if (tagged-list? rest 'in)
        (set! body (cdr rest))  ;basis tilfelle til rekursjonen
        (begin
          (set! vars (cons (cadr rest) vars))
          (set! args (cons (cadddr rest) args))
          (recur! (cddddr rest)))))
  (recur! exp)
  (build-lambda-exp vars body args))



;;; -------------------------- Oppgave (3e) -------------------------------
; Hvert while-uttrykk i vår implementasjon har 3 deler:
; 1. Et predikat
; 2. En liste av instruksjoner som vil bli utført hvis predikatet gir sann
; 3. En liste av oppdatering-operasjoner

(define (while? exp) (tagged-list? exp 'while))

(define (eval-while exp env)
  (if (true? (mc-eval (cadr exp) env))                  ;pred
      (begin (mc-eval (sequence->exp (caddr exp)) env)  ;body
             (mc-eval (sequence->exp (cadddr exp)) env) ;update
             (eval-while exp env))))



;;; -----------------------------------------------------------------------
;;special-form? fra prekoden blir utvidet slik:
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t)   ;ny (oppgave 3a)
        ((or? exp) #t)    ;ny (oppgave 3a)
        ((let? exp) #t)   ;ny (oppgave 3c-3d)
        ((while? exp) #t) ;ny (oppgave 3e)
        (else #f)))

;;eval-special-form fra prekoden blir utvidet slik:
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and exp env))       ;ny (oppgave 3a)
        ((or? exp) (eval-or exp env))         ;ny (oppgave 3a)
        ((let? exp) (eval-let exp env))       ;ny (oppgave 3c-3d)
        ((while? exp) (eval-while exp env)))) ;ny (oppgave 3e)


(display "\n============= Test for (2a) =============\n")
(mc-eval '(1+ 6) the-global-environment) ;7
(mc-eval '(1- 6) the-global-environment) ;5
(display "1+: ") (mc-eval '1+ the-global-environment)
(display "1-: ") (mc-eval '1+ the-global-environment)

(display "\n============= Test for (2b) =============\n")
(install-primitive! 'square (lambda (x) (* x x)))
(mc-eval '(square 10) the-global-environment) ;100
(display "square: ") (mc-eval 'square the-global-environment)

(display "\n============= Test for (3a) =============\n")
(mc-eval '(and 'true 'false undefined) the-global-environment) ;#f
(mc-eval '(and 'no 'and 'yes) the-global-environment)          ;yes
(mc-eval '(or 'false 'false 'false) the-global-environment)    ;#f
(mc-eval '(or 'false 'yes undefined) the-global-environment)   ;yes

(display "\n============= Test for (3b) =============\n")
(mc-eval '(if 'true 'yes undefined) the-global-environment)   ;yes
(mc-eval '(if #f then 'no elseif #t then 'yes else undefined) ;yes
         the-global-environment)
(mc-eval '(if #f then 'no elseif #f then 'nei else 'yes)      ;yes
         the-global-environment) 

(display "\n========== Test for (3c og 3d) ==========\n")
(mc-eval '(let ((x 3) (y 4)) (display "4 + 3 = ") (+ x y))
         the-global-environment)
(mc-eval '(let x = 4 and y = 3 in (display "4 - 3 = ") (- x y))
         the-global-environment)

(display "\n============= Test for (3e) =============\n")
(install-primitive! '< <) 
(install-primitive! '> >)
(install-primitive! '<= <=)
(install-primitive! '>= >=)
(mc-eval '(define x 0) the-global-environment) ;ok
(mc-eval '(define y 9) the-global-environment) ;ok
(define loop '(while (and (< x 10) (>= y 7))
                     ((display x) (display " ") (display y) (display " _ "))
                     ((set! x (+ x 1)) (set! y (- y 1)))))
(mc-eval loop the-global-environment) ;0 9 _ 1 8 _ 2 7

