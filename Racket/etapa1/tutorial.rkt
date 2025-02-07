#lang racket

; ----- Structuri -----
; -----  Etapa 1  -----

; Definirea unei structuri counter, cu trei câmpuri, index, tt și queue.
; Transparent permite afișarea valorilor câmpurilor.
(define-struct counter (index tt queue) #:transparent)

; Definirea unui obiect structură.
"Definire"
(define C1 (make-counter 1 2 '((ion . 1))))
(define C3 (make-counter 3 1 '((ion . 3))))
C1

; Accesul la câmpuri se face prin funcțiile <structură>-<câmp>.
"Acces"
(counter-index C1)
(counter-tt C1)
(counter-queue C1)

; „Modificarea” câmpurilor index și tt. Câmpurile nemenționate explicit
; își mențin valoarea.
; Atragem din nou atenția că nu există nicio modificare propriu-zisă
; a obiectului existent, ci se creează un nou obiect, cu noile valori
; ale câmpurilor.
"Modificare"
(define C2 (struct-copy counter C1 [index 2] [tt 4]))
C1
C2

; Există și forme speciale ce permit pattern matching la nivel de structuri.
; Putem folosi match pentru a extrage câmpurile unui obiect de tip counter astfel:
"Match care întoarce tt"
(match C1 [(counter index tt queue) tt])

; Underscore permite ignorarea anumitor câmpuri pe care nu le folosim.
; Expresia de mai sus e echivalentă cu:
"Match care întoarce queue"
(match C1 [(counter _ _ queue) queue])

; -----  Pentru următoarele etape  -----

; Există o formă specială de let care permite radiografierea unei structuri,
; cu evidențierea câmpurilor din interior.
"Match-let"
(match-let ([(counter index tt queue) C1]) queue)

; Există și o formă specială de lambda, care permite radiografierea
; parametrului funcției.


; Echivalent cu
(filter (lambda (C) (> (counter-tt C) 5)) (list C1 C2))


(define (sort-by-et lst) 
  (define (object-greater? a b)
    (<= (match-let ([(counter index tt queue) a]) tt) (match-let ([(counter index tt queue) b]) tt) ))
  (sort lst object-greater?))


(define counter-out
  (λ (C) (cons (counter-index C) (counter-queue C))))

(define counters (list C1 C2 C3))
