#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index tt et queue) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0 empty-queue))


(define (tt+ C minutes)
  (define old-tt (counter-tt C))
  (define C2 (struct-copy counter C [tt (+ old-tt minutes)]))
  C2)

(define (et+ C minutes)
  (define old-et (counter-et C))
  (define C2 (struct-copy counter C [et (+ old-et minutes)]))
  C2)

(define (add-to-counter name items)     
  (λ (C)                                
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

(define (remove-first-from-counter C)  
  (define (get-tt-sum Q acc)
    (if (stream-empty? Q)
        acc
        (+ acc (get-tt-sum (stream-rest Q) (cdr (stream-first Q))))))


  (define C-copy (if (queue-empty? (counter-queue C))
                     (if (stream-empty? (rotate (queue-left (counter-queue C)) (queue-right (counter-queue C)) empty-stream))
                         C
                         (struct-copy counter C [queue (dequeue (rotate (queue-left (counter-queue C)) (queue-right (counter-queue C)) empty-stream))]))
                     (struct-copy counter C [queue (dequeue (counter-queue C))])))
  
  (define old-queue (queue-left (counter-queue C-copy)))
  (define new-tt (if (stream-empty? old-queue) 0 (get-tt-sum old-queue 0)))
  (define new-et (if (stream-empty? old-queue) 0 (cdr (stream-first old-queue))))
  (define C2 (struct-copy counter C-copy [tt new-tt] [et new-et] [queue (counter-queue C-copy)]))
  C2)


(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C [tt ((pass-minutes minutes) (counter-tt C))] [et ((pass-minutes minutes) (counter-et C))])))

(define (pass-minutes minutes)
  (λ (x) (if (< (- x minutes) 0)
             0
             (- x minutes))))
  
  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
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
      (if (queue-empty? (counter-queue C))
          0
          (if (stream-empty? (rotate (queue-left (counter-queue C)) (queue-right (counter-queue C)) empty-stream))
              0
              (if (stream-empty? (stream-rest (rotate (queue-left (counter-queue C)) (queue-right (counter-queue C)) empty-stream)))
                  0
                  (cdr (stream-first (stream-rest  (rotate (queue-left (counter-queue C)) (queue-right (counter-queue C)) empty-stream))))))))

    (define to-add (if (queue-empty? (counter-queue C))
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
  (define (name-case req f-counters s-counters name n-items exit closed-counters)
    (define available-all (filter (lambda (C) ((negate member) (counter-index C) closed-counters)) (append f-counters s-counters)))
    (define available-slow (filter (lambda (C) ((negate member) (counter-index C) closed-counters)) s-counters))
    
    (define tt-min-all (min-tt available-all))
    (define tt-min-slow (min-tt available-slow))
    (define element-1 (car (filter (lambda (C) (= (counter-index C) (car tt-min-all))) available-all)))
    (define new-element-1 ((add-to-counter name n-items) element-1))
      
    (define element-2 (car (filter (lambda (C) (= (counter-index C) (car tt-min-slow))) available-slow)))
    (define new-element-2 ((add-to-counter name n-items) element-2))

    (if (<= n-items ITEMS)
        (serve-2 (cdr req) (replace-element f-counters element-1 new-element-1 '()) (replace-element s-counters element-1 new-element-1 '()) exit closed-counters)       
        (serve-2 (cdr req) (replace-element f-counters element-2 new-element-2 '()) (replace-element s-counters element-2 new-element-2 '()) exit closed-counters)))
     
  ;; DELAY
  (define (delay-case req f-counters s-counters index minutes exit closed-counters)
    (define element (car (filter (lambda (C) (= (counter-index C) index)) (append f-counters s-counters))))
    (define new-et (+ (counter-et element) minutes)) 
    (define new-element (struct-copy counter element [tt (+ (counter-tt element) minutes)] [et new-et]))
    
    (serve-2 (cdr req) (replace-element f-counters element new-element '()) (replace-element s-counters element new-element '()) exit closed-counters))

  
  ;; ENSURE
  (define (ensure-case req f-counters s-counters average exit closed-counters)


    (define available-all (filter (lambda (C) ((negate member) (counter-index C) closed-counters)) (append f-counters s-counters)))

    (define C (make-counter (+ (length (append f-counters s-counters)) 1) 0 0 empty-queue))

    (define (get-sum L acc)
      (if (null? L)
          acc
          (get-sum (cdr L) (+ acc (counter-tt (car L))))))
     
    (if (> (/ (get-sum available-all 0) (length available-all)) average)
        (ensure-case req f-counters (reverse (cons C (reverse s-counters))) average exit closed-counters) 
        (serve-2 (cdr req) f-counters s-counters exit closed-counters)))

  
  ;; <X> MINUTES
  (define (x-minutes-case req f-counters s-counters minutes exit closed-counters)

    (define exit-2 (car (counter-list-op (sort-by-et (append f-counters s-counters)) 1 '() '())))
            
    (if (= minutes 0)
        (serve-2 (cdr req) f-counters s-counters exit closed-counters)
        (x-minutes-case req (restore-list (cdr (counter-list-op (sort-by-et (append f-counters s-counters)) 1 '() '())) f-counters '())
                        (restore-list (cdr (counter-list-op (sort-by-et (append f-counters s-counters)) 1 '() '())) s-counters '()) (sub1 minutes) (append exit exit-2) closed-counters))) 

  ;; CLOSE 
  (define (close-case req f-counters s-counters index exit closed-counters)
    (serve-2 (cdr req) f-counters s-counters exit (cons (counter-index (car (filter (lambda (C) (= (counter-index C) index)) (append f-counters s-counters)))) closed-counters)))

  (define counter-out-format
    (λ (C) (cons (counter-index C) (counter-queue C))))

  ;; SERVE - HELPER
  (define (serve-2 req f-counters s-counters out-order closed-counters)
    (if (null? req)
        (cons out-order (map counter-out-format (filter (lambda (C) ((negate queue-empty?) (counter-queue C))) (append f-counters s-counters))))
        (match (car req)
          [(list 'close index)         (close-case req f-counters s-counters index out-order closed-counters)]
          [(list 'delay index minutes) (delay-case req f-counters s-counters index minutes out-order closed-counters)]
          [(list 'ensure average)      (ensure-case req f-counters s-counters average out-order closed-counters)]
          [(list name n-items)         (name-case req f-counters s-counters name n-items out-order closed-counters)]
          [ minutes                    (x-minutes-case req f-counters s-counters minutes out-order closed-counters)])))

  (serve-2 requests fast-counters slow-counters '() '()))