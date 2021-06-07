#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (define C (make-counter index 0 0 empty-queue))
  C)


(define (update f counters index)
  (define (length lst)
    (cond
      [(empty? lst)  0]
      [(cons? lst)   (+ 1 (length (rest lst)))]))

  (define (compute-result f counters index acc)
    (cond
      [(null? counters) (reverse acc)]
      [(= (counter-index (car counters)) index) (cons (f (car counters)) (compute-result f (cdr counters) index acc))]
      [else (cons (car counters) (compute-result f (cdr counters) index acc))]))
  
  (cond 
    [(null? counters) '()]
    [(< index 0) counters]
    [(> index (length counters)) counters]
    [else (compute-result f counters index '())]))

(define (tt+ C minutes)
  (define old-tt (counter-tt C))
  (define C2 (struct-copy counter C [tt (+ old-tt minutes)]))
  C2)

(define (et+ C minutes)
  (define old-et (counter-et C))
  (define C2 (struct-copy counter C [et (+ old-et minutes)]))
  C2)

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (tt+ (struct-copy counter C [et (get-new-et C items)] [queue (enqueue (cons name items) (counter-queue C))]) items)))

(define (get-new-et C items)
  (define old-et (counter-et C))
  (define new-et (if (queue-empty? (counter-queue C))
                     (+ old-et items)
                     old-et))
  new-et)

(define (apply-min-tt-or-et field-type min-value index L)
  (cond 
    ((null? L) (cons index min-value))
    ((< (field-type (car L)) min-value) (apply-min-tt-or-et field-type (field-type (car L)) (counter-index (car L)) (cdr L)))
    (else (apply-min-tt-or-et field-type min-value index (cdr L)))))

(define (min-tt  L)
  (apply-min-tt-or-et counter-tt +inf.0 0 L))

(define (min-et  L)
  (apply-min-tt-or-et counter-et +inf.0 0 L)) 

(define (remove-first-from-counter C)   ; testată de checker
  (define (get-tt-sum L acc)
    (if (null? L)
        acc
        (+ acc (get-tt-sum (cdr L) (cdr (car L))))))

  (define C-copy (struct-copy counter C [queue (dequeue (counter-queue C))]))
  (define old-queue (queue-left (counter-queue C-copy)))
  (define new-tt (if (null? old-queue) 0 (get-tt-sum old-queue 0)))
  (define new-et (if (null? old-queue) 0 (cdr (car old-queue))))
  (define C2 (struct-copy counter C-copy [tt new-tt] [et new-et] [queue (counter-queue C-copy)]))
  C2)


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C [tt ((pass-minutes minutes) (counter-tt C))] [et ((pass-minutes minutes) (counter-et C))])))

(define (pass-minutes minutes)
  (λ (x) (if (< (- x minutes) 0)
             0
             (- x minutes))))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve requests fast-counters slow-counters)

  ;; HELPER FUNCTIONS
  (define (my-match L idx)
    (filter (match-lambda [(counter index _ _ _) (= index idx)]) L))

  (define (restore-list L-all L L-selected)
    (if (null? L)
        (reverse L-selected)
        (restore-list L-all (cdr L) (cons (car (my-match L-all (counter-index (car L)))) L-selected))))

  (define (sort-by-et lst) 
    (define (object-greater? a b)
      (< (match-let ([(counter index tt et queue) a]) et) (match-let ([(counter index tt et queue) b]) et) ))
    (sort lst object-greater?))

  (define (sort-asc-by-first L)
    (sort L
          (lambda (x y) (< (car x) (car y)))))
  
  (define (counter-operation C minutes acc)
    (define et (counter-et C))
  
    (define new-et
      (if (null? (top (dequeue (counter-queue C))))
          0
          (cdr (top (dequeue (counter-queue C))))))

    (define to-add (if (null? (top (counter-queue C)))
                       acc
                       (reverse (cons (cons (counter-index C) (car (top (counter-queue C)))) (reverse acc)))))            

    (cond
      [(queue-empty? (counter-queue C)) (cons to-add ((pass-time-through-counter minutes) C))]
      [(> minutes et) (counter-operation (remove-first-from-counter C) (- minutes et)  to-add)]
      [(= minutes et) (cons to-add (remove-first-from-counter C))]
      [else (cons acc ((pass-time-through-counter minutes) C))]))

  
  (define (counter-list-op L minutes exit new-L)
    (if (null? L)
        (cons exit (reverse new-L))
        (counter-list-op (cdr L) minutes (append exit (car (counter-operation (car L) minutes '()))) (cons (cdr (counter-operation (car L) minutes '())) new-L))))

  
  ;; REPLACE ELEMENT IN LIST
  (define (replace-element L elem new-elem acc)
    (cond
      ((null? L) (reverse acc))
      ((equal? (car L) elem) (cons new-elem (replace-element (cdr L) elem new-elem acc))) 
      (else (cons (car L) (replace-element (cdr L) elem new-elem acc)))))

  ;; NAME
  (define (name-case req f-counters s-counters name n-items exit)
    (define tt-min-all (min-tt (append f-counters s-counters)))
    (define tt-min-slow (min-tt s-counters))
    (define element-1 (car (filter (lambda (C) (= (counter-index C) (car tt-min-all))) (append f-counters s-counters))))
    (define new-element-1 ((add-to-counter name n-items) element-1))
      
    (define element-2 (car (filter (lambda (C) (= (counter-index C) (car tt-min-slow))) s-counters)))
    (define new-element-2 ((add-to-counter name n-items) element-2))

    (if (<= n-items ITEMS)
        (serve-2 (cdr req) (replace-element f-counters element-1 new-element-1 '()) (replace-element s-counters element-1 new-element-1 '()) exit)       
        (serve-2 (cdr req) (replace-element f-counters element-2 new-element-2 '()) (replace-element s-counters element-2 new-element-2 '()) exit)))
     
  ;; DELAY
  (define (delay-case req f-counters s-counters index minutes exit)
    (define element (car (filter (lambda (C) (= (counter-index C) index)) (append f-counters s-counters))))
    (define new-et (+ (counter-et element) minutes)) 
    (define new-element (struct-copy counter element [tt (+ (counter-tt element) minutes)] [et new-et]))
    
    (serve-2 (cdr req) (replace-element f-counters element new-element '()) (replace-element s-counters element new-element '()) exit))

  
  ;; ENSURE
  (define (ensure-case req f-counters s-counters average exit)

    (define C (make-counter (+ (length (append f-counters s-counters)) 1) 0 0 empty-queue))

    (define (get-sum L acc)
      (if (null? L)
          acc
          (get-sum (cdr L) (+ acc (counter-tt (car L))))))
     
    (if (> (/ (get-sum (append f-counters s-counters) 0) (length (append f-counters s-counters))) average)
        (ensure-case req f-counters (reverse (cons C (reverse s-counters))) average exit) 
        (serve-2 (cdr req) f-counters s-counters exit)))

  
  ;; <X> MINUTES
  (define (x-minutes-case req f-counters s-counters minutes exit)
    (define exit-2 (car (counter-list-op (sort-by-et (append f-counters s-counters)) minutes '() '())))
   
  
    (serve-2 (cdr req) (restore-list (cdr (counter-list-op (sort-by-et (append f-counters s-counters)) minutes '() '())) f-counters '())
             (restore-list (cdr (counter-list-op (sort-by-et (append f-counters s-counters)) minutes '() '())) s-counters '()) (append exit exit-2))) 


  ;; SERVE - HELPER
  (define (serve-2 req f-counters s-counters out-order)
    (if (null? req)
        (cons out-order (append f-counters s-counters))
        (match (car req)
          [(list 'delay index minutes) (delay-case req f-counters s-counters index minutes out-order)]
          [(list 'ensure average)      (ensure-case req f-counters s-counters average out-order)]
          [(list name n-items)         (name-case req f-counters s-counters name n-items out-order)]
          [ minutes                    (x-minutes-case req f-counters s-counters minutes out-order)])))

  (serve-2 requests fast-counters slow-counters '()))
