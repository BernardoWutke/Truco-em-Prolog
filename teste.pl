:- dynamic soma/2.

:- dynamic pontos/1.
:- dynamic lista/1.
:- dynamic baralho_1/1.
:- dynamic baralho_2/1.
:- dynamic baralho_buff/1.

pontos(2).


truco():-
    pontos(PONTOS_RODADA),
    (
        PONTOS_RODADA == 2 ->(
            NEW_PONTOSRODADA is PONTOS_RODADA + 2,
            retract(pontos(PONTOS_RODADA)),
            assert(pontos(NEW_PONTOSRODADA))
        );
        PONTOS_RODADA == 4 ->(
            NEW_PONTOSRODADA is PONTOS_RODADA + 2,
            retract(pontos(PONTOS_RODADA)),
            assert(pontos(NEW_PONTOSRODADA))
        );
        PONTOS_RODADA == 6 ->(
            NEW_PONTOSRODADA is PONTOS_RODADA + 4,
            retract(pontos(PONTOS_RODADA)),
            assert(pontos(NEW_PONTOSRODADA))
        );
        PONTOS_RODADA == 10 ->(
            NEW_PONTOSRODADA is PONTOS_RODADA + 2,
            retract(pontos(PONTOS_RODADA)),
            assert(pontos(NEW_PONTOSRODADA))
        )
    ).
    


soma(0,0).

somar(X):- soma(Y,Z), Y1 is Y+X, retract(soma(Y,Z)), asserta(soma(Y1,Y1)).


aleatorio():-
        random(1,40,X),
        write(X).
        

lista_cartas([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40]).

baralho([carta(4-paus), carta(5-paus), carta(6-paus), carta(7-paus), carta(q-paus), carta(j-paus), carta(k-paus), carta(a-paus), carta(2-paus), carta(3-paus), carta(4-copas), carta(5-copas), carta(6-copas), carta(7-copas), carta(q-copas), carta(j-copas), carta(k-copas), carta(a-copas), carta(2-copas), carta(3-copas), carta(4-espadas), carta(5-espadas), carta(6-espadas), carta(7-espadas), carta(q-espadas), carta(j-espadas), carta(k-espadas), carta(a-espadas), carta(2-espadas), carta(3-espadas), carta(4-ouros), carta(5-ouros), carta(6-ouros), carta(7-ouros), carta(q-ouros), carta(j-ouros), carta(k-ouros), carta(a-ouros), carta(2-ouros), carta(3-ouros)]).
baralho_buff([carta(4-paus), carta(5-paus), carta(6-paus), carta(7-paus), carta(q-paus), carta(j-paus), carta(k-paus), carta(a-paus), carta(2-paus), carta(3-paus), carta(4-copas), carta(5-copas), carta(6-copas), carta(7-copas), carta(q-copas), carta(j-copas), carta(k-copas), carta(a-copas), carta(2-copas), carta(3-copas), carta(4-espadas), carta(5-espadas), carta(6-espadas), carta(7-espadas), carta(q-espadas), carta(j-espadas), carta(k-espadas), carta(a-espadas), carta(2-espadas), carta(3-espadas), carta(4-ouros), carta(5-ouros), carta(6-ouros), carta(7-ouros), carta(q-ouros), carta(j-ouros), carta(k-ouros), carta(a-ouros), carta(2-ouros), carta(3-ouros)]).


lista([1,2,3]).

add_element(X):-
    lista(L),
    append(L,[X],L1),
    retract(lista(L)),
    asserta(lista(L1)).

lista_index():-
        random(1,40,Y),
        baralho(LISTA),
        nth0(Y, LISTA, X),
        write(X).

baralho_1([]).
baralho_2([]).

tamanho():-
    baralho_buff(LISTA),
    length(LISTA,X),
    write(X).

dar_cartas(CONTROLE):-
    baralho_1(LISTA),
    baralho_buff(LISTA_BARALHO),
    length(LISTA_BARALHO,TAMANHO_1),
    random(1,TAMANHO_1,X),
    nth0(X, LISTA_BARALHO, CARTA),
    append(LISTA, [CARTA], LISTA1),
    retract(baralho_1(LISTA)),
    asserta(baralho_1(LISTA1)),
    delete(LISTA_BARALHO, CARTA, LISTA_BARALHO1),
    retract(baralho_buff(LISTA_BARALHO)),
    asserta(baralho_buff(LISTA_BARALHO1)),
    baralho_2(LISTAP),
    baralho_buff(LISTA_BARALHOP),
    length(LISTA_BARALHOP,TAMANHO_2),
    random(1,TAMANHO_2,Y),
    nth0(Y, LISTA_BARALHOP, CARTA2),
    append(LISTAP, [CARTA2], LISTAP2),
    retract(baralho_2(LISTAP)),
    asserta(baralho_2(LISTAP2)),
    delete(LISTA_BARALHOP, CARTA2, LISTA_BARALHOP2),
    retract(baralho_buff(LISTA_BARALHOP)),
    asserta(baralho_buff(LISTA_BARALHOP2)),
    (
    CONTROLE < 3 ->(
        CONTROLE1 is CONTROLE + 1,
        write(CONTROLE1),
        dar_cartas(CONTROLE1)
    );
    CONTROLE >= 3 ->(
        write('Cartas distribuidas')
    )
    ).

jogar():-
    baralho_1(LISTA),
    write(LISTA),nl,
    write('Digite a carta que deseja jogar: (0,1,2) '),nl,
    read(X),
    nth0(X, LISTA, CARTA),
    write(CARTA),nl,
    delete(LISTA, CARTA, LISTA1),
    retract(baralho_1(LISTA)),
    asserta(baralho_1(LISTA1)).

    
e2(X):-
    X = 2.
    

fora():-
    e2(X),
    write(X).
