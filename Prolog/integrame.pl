:- ensure_loaded('checker.pl').

test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un srting, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de stringuri reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un string)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.

intrebari(integ( _ , _ , [], _ ), []). %% caz de baza
intrebari(integ( _ , _ , [( _ , x) | RestList], _) , Lista_intrebari):-
    intrebari(integ( _ , _ , RestList, _), Lista_intrebari). %% daca este celula neagra, trec mai departe

intrebari(integ( _ , _ , [((R,C), [(Text, Dir, ID) | RestLista]) | Rest], _ ), [((R, C), Text, Dir, ID) | RQ]) :- 
    intrebari(integ( _ , _ , [((R,C), RestLista) | Rest], _ ), RQ). %% daca am gasit intrebarea si lista are mai multe elemente
                                                                    %% avansez in ambele liste 

intrebari(integ( _ , _ , [((R,C), [(Text, Dir, ID)]) | Rest], _ ), [((R, C), Text, Dir, ID) | RQ]) :- 
    intrebari(integ( _ , _ , Rest, _ ), RQ). %% daca am gasit intrebarea si lista are un singur element
                                             %% avansez in lista de intrebari si trec la urmatorul (R, C)  

intrebari(integ( _ , _ , [((R,C), []) | Rest], _ ), [((R, C), Text, Dir, ID) | RQ]) :- 
    intrebari(integ( _ , _ , Rest, _ ), [((R, C), Text, Dir, ID) | RQ]). %% daca nu mai sunt elemente in lista, trec la urmatorul (R, C)


% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.

id_intrebare(integ( _ , _ , [( _ , [(Intrebare, _ , Q_ID) | _ ]) | _ ], _ ), Intrebare, Q_ID). %% caz de bază, am gasit intrebarea cu ID-ul dat
id_intrebare(integ( _ , _ , [( _ , x) | Rest], _ ), Intrebare, Q_ID) :- 
    id_intrebare(integ( _ , _ , Rest, _ ), Intrebare, Q_ID). %% daca este celula neagra, trec mai departe

id_intrebare(integ( _ , _ , [((R, C) , [( _ , _ , _ ) | RestLista]) | Rest], _ ), Intrebare, Q_ID) :- 
    id_intrebare(integ( _ , _ ,[((R, C) , RestLista) | Rest], _ ), Intrebare, Q_ID). %% daca intrebarea nu are ID-ul cautat trec la urmatoarea din lista

id_intrebare(integ( _ , _ ,[( _ , []) | Rest], _ ), Intrebare, Q_ID) :-
    id_intrebare(integ( _ , _ , Rest, _ ), Intrebare, Q_ID). %% daca lista de intrebari este goala, trec la urmatorul (R, C)

                                                                                  
% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvând
% de completat; ambele sunt stringuri.
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).


completare(Integ, Lista, Output) :- completareAcc(Integ, Lista, [], Output).

completareAcc(integ(H, W, [], Vocab), _, Acc, integ(H, W, FilteredAcc, Vocab)) :- set(Acc, FilteredAcc). %% eliminare duplicate

completareAcc(integ(H, W, [(Coord, Val) | Rest], Vocab), ListaPerechi, Acc, Integrama):-   % daca celula este completata, trec mai departe
    \+ is_list(Val),    
    completareAcc(integ(H, W, Rest, Vocab), ListaPerechi, [(Coord, Val) | Acc], Integrama). %% adaug perechea ((R,C), Val) la acumulator pt a nu o pierde 

completareAcc(integ(H, W, [((R,C), Val) | Rest], Vocab), ListaPerechi, Acc, Integrama):- % daca celula nu este completata
    is_list(Val),                                                                         
    adaugare(R, C, Val, ListaPerechi, ListaAcc),
    append(ListaAcc, Acc, NewAcc), %% adaug perechile de (coordonate, litera) la lista integramei
    completareAcc(integ(H, W, Rest, Vocab), ListaPerechi, [((R,C), Val)| NewAcc], Integrama).                

% listaJos/3
% listaJos(+Poz, +Litere, -Raspuns)
listaJos(_, [], []).       %% genereaza o lista de perechi (coordonate, litera) - listaJos((0,0), ['a', 'n', 'a'], R).-> R = [((1, 0), a),  ((2, 0), n),  ((3, 0), a)]
listaJos((R,C), [Litera | RestLitere], [((Row,C), Litera) | Rest]):-
    Row is R + 1,
    listaJos((Row, C), RestLitere, Rest).

% listaDreapta/3
% listaDreapta(+Poz, +Litere, -Raspuns)
listaDreapta(_, [], []).   %% genereaza o lista de perechi (coordonate, litera) - listaDreapta((0,0), ['a', 'n', 'a'], R). -> R = [((0, 1), a),  ((0, 2), n),  ((0, 3), a)]
listaDreapta((R,C), [Litera | RestLitere], [((R,Col), Litera) | Rest]):-
    Col is C + 1,
    listaDreapta((R, Col), RestLitere, Rest).

% extrRaspuns/3
% extrRaspuns(+Intrebare, +ListaPerechi, -Raspuns)
extrRaspuns(Intrebare, [(Intrebare, Raspuns) | _], Raspuns).
extrRaspuns(Text, [ _ | Rest], Raspuns):- extrRaspuns(Text, Rest, Raspuns).

adaugare(_, _, [], _, []).
adaugare(R, C, [(Text, _, _) | Rest], ListaPerechi, ListaAcc) :-
    \+ member((Text, _), ListaPerechi), %% daca intrebarea nu se afla in lista de perechi (Intrebare, Raspuns), trec la urmatoarea 
    adaugare(R,C, Rest, ListaPerechi, ListaAcc).

adaugare(R, C, [(Text, Dir, _) | Rest], ListaPerechi, NewListaAcc) :-
    member((Text, _), ListaPerechi), %% intrebarea se afla in lista de perechi (Intrebare, Raspuns) 
    extrRaspuns(Text, ListaPerechi, Raspuns),  %% extrag raspunsul corespunzator din lista de perechi (Intrebare, Raspuns) -> Raspuns
    Dir == d,        %% verific in ce directie trebuie sa generez perechile
    atom_chars(Raspuns, RaspLista), %% transform din string in lista de caractere
    listaDreapta((R, C), RaspLista, Result),   %% generez perechile de (coordonate, litera)
    append(Result, ListaAcc, NewListaAcc),
    adaugare(R,C, Rest, ListaPerechi, ListaAcc). 

adaugare(R, C, [(Text, Dir, _) | Rest], ListaPerechi, NewListaAcc) :-
    member((Text, _), ListaPerechi),
    extrRaspuns(Text, ListaPerechi, Raspuns),
    Dir == j,
    atom_chars(Raspuns, RaspLista),
    listaJos((R, C), RaspLista, Result),
    append(Result, ListaAcc, NewListaAcc),
    adaugare(R,C, Rest, ListaPerechi, ListaAcc).


%% eliminare duplicate
set([], []).
set([H|T], [H|T1]) :- 
    remv(H, T, T2),
    set(T2, T1).

remv(_, [], []).
remv(X, [X|T], T1) :- remv(X, T, T1).
remv(X, [H|T], [H|T1]) :-
    X \= H,
    remv(X, T, T1).

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).

lungime_spatiu(integ(H, W, [((R,C), [(Intrebare, Dir, ID) | _]) | Rest], _ ), Intrebare, Lungime) :- 
    Dir == j,
    lungime_jos(integ(H, W, [((R,C), [(Intrebare, Dir, ID) | _]) | Rest], _ ), (R, C), Lungime). 

lungime_spatiu(integ(H , W, [((R,C), [(Intrebare, Dir, ID) | _]) | Rest], _ ), Intrebare, Lungime) :- 
    Dir == d,
    lungime_dreapta(integ(H, W, [((R,C), [(Intrebare, Dir, ID) | _]) | Rest], _ ), (R, C), Lungime).

lungime_spatiu(integ(H, W, [((_, _), x) | Rest], Vocab), Intrebare, Lungime):-
    lungime_spatiu(integ(H, W, Rest, Vocab), Intrebare, Lungime).  %% daca este o celula neagra, merg mai departe

lungime_spatiu(integ(H, W, [((R,C), [(_, _, _) | RestLista]) | Rest], _ ), Intrebare, Lungime) :- 
    lungime_spatiu(integ(H, W, [((R,C), RestLista) | Rest], _ ), Intrebare, Lungime). %% daca este intrebarea din lista nu este cea cautata, merg la urmatoarea

lungime_spatiu(integ(H, W, [((_, _), []) | Rest], _ ), Intrebare, Lungime) :- 
    lungime_spatiu(integ(H, W, Rest, _ ), Intrebare, Lungime).   %% daca lista de intrebari este goala, trec la urmatorul (R, C)

% lungime_jos/3
% lungime_jos(integ(+H, +W, +Lista, +Vocab), +StartPos, -Lungime)
lungime_jos(integ(_,_,[((Rn, C), _) | _], _), (R, C), Res):- 
    Rn > R,
    !,
    Res is Rn - R - 1.
    
lungime_jos(integ(_,_,[((_, _), _) | Rest], _), (R, C), Lungime) :-   %% plec de la coordonatele (R, C) si merg in jos pana dau de X sau o lista de intrebari
    lungime_jos(integ(_, _, Rest, _), (R, C), Lungime).               

% lungime_dreapta/3
% lungime_dreapta(integ(+H, +W, +Lista, +Vocab), +StartPos, -Lungime)
lungime_dreapta(integ(_,_,[((R, Cn), _) | _], _), (R, C), Res):- 
    Cn > C,
    !,
    Res is Cn - C - 1.
    
lungime_dreapta(integ(_,_,[((_, _), _) | Rest], _), (R, C), Lungime) :-   %% plec de la coordonatele (R, C) si merg in dreapta pana dau de X sau o lista de intrebari
    lungime_dreapta(integ(_, _, Rest, _), (R, C), Lungime).

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
intersectie(Integ, I1, Poz1, I2, Poz2) :-
    raspuns(Integ, I1, R1),  %% R1 - lista de coordonate pt literele intrebarii 1
    raspuns(Integ, I2, R2),  %% R2 - lista de coordonate pt literele intrebarii 2
    I1 \= I2,           %% intrebarile sa fie diferite
    intersection(R1, R2, [Prim | _]),  %% extrag coordonatele unde se intersecteaza cele 2 raspunsuri
    nth0(Poz1, R1, Prim),  %% iau pozitia din lista R1 la care se afla intersectia
    nth0(Poz2, R2, Prim).  %% iau pozitia din lista R2 la care se afla intersectia

raspuns(Integ, Intrebare, R) :-
    directie_pozitie(Integ, Intrebare, Q_Dir, Q_Pos), %% returneaza directia si pozitia unei intrebari
    lungime_spatiu(Integ, Intrebare, Lungime), %% returneaza lungimea alocata raspunsului intrebarii
    genereaza(Q_Pos, Lungime, Q_Dir, R). %% intoarce o lista de coordonate care corespund fiecarei litere din raspuns

directie_pozitie(integ( _ , _ , [(Q_Pos , [(Intrebare, Q_Dir, _) | _ ]) | _ ], _ ), Intrebare, Q_Dir, Q_Pos).

directie_pozitie(integ( _ , _ , [( _ , x) | Rest], _ ), Intrebare, Q_Dir, Q_Pos) :- 
    directie_pozitie(integ( _ , _ , Rest, _ ), Intrebare, Q_Dir, Q_Pos).

directie_pozitie(integ( _ , _ , [((R, C), [( _ , _ , _ ) | RestLista]) | Rest], _ ), Intrebare, Q_Dir, Q_Pos) :- 
    directie_pozitie(integ( _ , _ ,[((R, C), RestLista) | Rest], _ ), Intrebare, Q_Dir, Q_Pos).

directie_pozitie(integ( _ , _ , [( _ , []) | Rest], _ ), Intrebare, Q_Dir, Q_Pos) :- 
    directie_pozitie(integ( _ , _ , Rest, _ ), Intrebare, Q_Dir, Q_Pos).

genereaza(_, 0, _, []).

genereaza((R,C), Lungime, j, [ (Row, C) | Rest]):-
    NLungime is Lungime - 1,
    Row is R + 1,
    genereaza((Row, C), NLungime, j, Rest),
    !.

genereaza((R,C), Lungime, d, [(R, Col) | Rest]):-
    NLungime is Lungime - 1,
    Col is C + 1,
    genereaza((R, Col), NLungime, d, Rest),
    !.


% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de stringuri, fiecare string
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])


solutii_posibile(Integ, Solutii):- 
    intrebari(Integ, Lista_intrebari),
    sol_posibile(Integ, Lista_intrebari, Solutii). %% pastrez integrama initiala

sol_posibile(_, [], []).

sol_posibile(integ(H, W, Lista, Vocab), [(_, Text, _, _) | Rest], [(Text, ListaRaspunsuri) | Solutii]):-
    lungime_spatiu(integ(H, W, Lista, Vocab), Text, Lungime),
    genereaza_raspunsuri(Lungime, Vocab, ListaRaspunsuri),
    sol_posibile(integ(H, W, Lista, Vocab), Rest, Solutii),
    !.

genereaza_raspunsuri(_, [], []).

genereaza_raspunsuri(Lungime, [Raspuns | Rest], [Raspuns | Sol]):- %% daca lungimile coincid, adaug cuvantul in lista
    atom_length(Raspuns, Lun),
    Lungime == Lun,
    genereaza_raspunsuri(Lungime, Rest, Sol),
    !. 

genereaza_raspunsuri(Lungime, [Raspuns | Rest], Sol):-
    atom_length(Raspuns, Lun),
    Lungime =\= Lun,
    genereaza_raspunsuri(Lungime, Rest, Sol),
    !.

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de stringuri, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca string) care este
% răspunsul la întrebare.
rezolvare(_, _) :- false.

