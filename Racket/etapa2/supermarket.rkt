#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (define C (make-counter index 0 0 '()))
  C)


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.

(define (length lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))]))

(define (update f counters index)

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

; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define (tt+ C minutes)
  (define old-tt (counter-tt C))
  (define C2 (struct-copy counter C [tt (+ old-tt minutes)]))
  C2)




; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define (et+ C minutes)
  (define old-et (counter-et C))
  (define C2 (struct-copy counter C [et (+ old-et minutes)]))
  C2)



; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define (add-to-counter C name n-items)
  (define old-queue (counter-queue C))
  (define old-tt (counter-tt C))
  (define old-et (counter-et C))
  (define new-et (if (null? (counter-queue C))
                     (+ old-et n-items)
                     old-et))

  (define C2 (struct-copy counter C [queue (reverse (cons (cons name n-items)  (reverse old-queue)))] [tt (+ old-tt n-items)] [et new-et]))
  C2)


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)

(define (apply-min-tt-or-et field-type min-value index L)
  (cond 
    ((null? L) (cons index min-value))
    ((< (field-type (car L)) min-value) (apply-min-tt-or-et field-type (field-type (car L)) (counter-index (car L)) (cdr L)))
    (else (apply-min-tt-or-et field-type min-value index (cdr L)))))

(define (min-tt  L)
  (apply-min-tt-or-et counter-tt +inf.0 0 L)) ; folosind funcția de mai sus
(define (min-et  L)
  (apply-min-tt-or-et counter-et +inf.0 0 L)) ; folosind funcția de mai sus


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.

(define (remove-first-from-counter C)
  (define (get-tt-sum L acc)
    (if (null? L)
        acc
        (+ acc (get-tt-sum (cdr L) (cdr (car L))))))
  
  (define old-queue (counter-queue C))
  (define new-tt (if (null? (cdr old-queue)) 0 (get-tt-sum (cdr old-queue) 0)))
  (define new-et (if (null? (cdr old-queue)) 0 (cdr (cadr old-queue))))
  (define C2 (struct-copy counter C [tt new-tt] [et new-et] [queue (cdr old-queue)]))
  C2)
    

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)
(define (serve requests fast-counters slow-counters)

  (define (replace-element L elem new-elem acc)
    (cond
      ((null? L) (reverse acc))
      ((equal? (car L) elem) (cons new-elem (replace-element (cdr L) elem new-elem acc))) 
      (else (cons (car L) (replace-element (cdr L) elem new-elem acc)))))
  
  (define (delay-case req f-counters s-counters index minutes)
    (define element (car (filter (lambda (C) (= (counter-index C) index)) (append f-counters s-counters))))
    (define new-et (+ (counter-et element) minutes)) 
    (define new-element (struct-copy counter element [tt (+ (counter-tt element) minutes)] [et new-et]))
  
    (serve (cdr req) (replace-element f-counters element new-element '()) (replace-element s-counters element new-element '())))
    
    
  
  (define (name-case req f-counters s-counters name n-items)
    (define tt-min-all (min-tt (append f-counters s-counters)))
    (define tt-min-slow (min-tt s-counters))
    (define element-1 (car (filter (lambda (C) (= (counter-index C) (car tt-min-all))) (append f-counters s-counters))))
    (define new-element-1 (add-to-counter element-1 name n-items))
    (define element-2 (car (filter (lambda (C) (= (counter-index C) (car tt-min-slow))) s-counters)))
    (define new-element-2 (add-to-counter element-2 name n-items))

    (if (<= n-items ITEMS)
        (serve (cdr req) (replace-element f-counters element-1 new-element-1 '()) (replace-element s-counters element-1 new-element-1 '()))       
        (serve (cdr req) (replace-element f-counters element-2 new-element-2 '()) (replace-element s-counters element-2 new-element-2 '()))))
    
  
  (define (remove-first-case req f-counters s-counters)
   
    (define search-domain (filter (lambda (C) (equal? (null? (counter-queue C)) #f)) (append f-counters s-counters)))
    (define et-min (min-et search-domain))
    (define element (filter (lambda (C) (= (counter-index C) (car et-min))) (append f-counters s-counters)))
     
    (cond
      [(null? element)                       (serve (cdr req) f-counters s-counters)]
      [(null? (counter-queue (car element))) (serve (cdr req) f-counters s-counters)]
      [else (serve (cdr req) (replace-element f-counters (car element) (remove-first-from-counter (car element)) '()) (replace-element s-counters (car element) (remove-first-from-counter (car element)) '()))]))

  (define (ensure-case req f-counters s-counters average)
    (define C (make-counter (+ (length (append f-counters s-counters)) 1) 0 0 '()))

    (define (get-sum L acc)
      (if (null? L)
          acc
          (get-sum (cdr L) (+ acc (counter-tt (car L))))))
     
    (if (> (/ (get-sum (append f-counters s-counters) 0) (length (append f-counters s-counters))) average)
        (ensure-case req f-counters (reverse (cons C (reverse s-counters))) average) 
        (serve (cdr req) f-counters s-counters)))
  
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'delay index minutes) (delay-case requests fast-counters slow-counters index minutes)]
        [(list 'remove-first)        (remove-first-case requests fast-counters slow-counters)]
        [(list 'ensure average)      (ensure-case requests fast-counters slow-counters average)]
        [(list name n-items)         (name-case requests fast-counters slow-counters name n-items)])))
