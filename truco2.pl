%variaveis
:- (dynamic pontos/2).
:- (dynamic ponstos_rodada/1).
:- (dynamic trucado/2).
:- (dynamic mao/1).
:- (dynamic rodadas_ganhas/2).
:- (dynamic pontos_mao/2).
:- (dynamic baralho_1/1).
:- (dynamic baralho_2/1).
:- (dynamic baralho_buff/1).
:- (dynamic carta_atual_1/1).
:- (dynamic carta_atual_2/1).

carta_atual_1(zero).
carta_atual_2(zero).

trucado(0, 1).

mao(1).

rodadas_ganha(jogador(1), 0).
rodadas_ganha(jogador(2), 0).

pontos(jogador(1), 0).
pontos(jogador(2), 0).

pontos_mao(jogador(1), 0).
pontos_mao(jogador(2), 0).

ponstos_rodada(2).
%fatos
jogador(1).
jogador(2).

carta(4-paus).
carta(5-paus).
carta(6-paus).
carta(7-paus).
carta(q-paus).
carta(j-paus).
carta(k-paus).
carta(a-paus).
carta(2-paus).
carta(3-paus).

carta(4-copas).
carta(5-copas).
carta(6-copas).
carta(7-copas).
carta(q-copas).
carta(j-copas).
carta(k-copas).
carta(a-copas).
carta(2-copas).
carta(3-copas).

carta(4-espadas).
carta(5-espadas).
carta(6-espadas).
carta(7-espadas).
carta(q-espadas).
carta(j-espadas).
carta(k-espadas).
carta(a-espadas).
carta(2-espadas).
carta(3-espadas).

carta(4-ouros).
carta(5-ouros).
carta(6-ouros).
carta(7-ouros).
carta(q-ouros).
carta(j-ouros).
carta(k-ouros).
carta(a-ouros).
carta(2-ouros).
carta(3-ouros).



valor(carta(zero),0).



valor(carta(5-paus), 2).
valor(carta(6-paus), 3).
valor(carta(7-paus), 4).
valor(carta(q-paus), 5).
valor(carta(j-paus), 6).
valor(carta(k-paus), 7).
valor(carta(a-paus), 8).
valor(carta(2-paus), 9).
valor(carta(3-paus), 10).

valor(carta(4-copas), 1).
valor(carta(5-copas), 2).
valor(carta(6-copas), 3).

valor(carta(q-copas), 5).
valor(carta(j-copas), 6).
valor(carta(k-copas), 7).
valor(carta(a-copas), 8).
valor(carta(2-copas), 9).
valor(carta(3-copas), 10).

valor(carta(4-espadas), 1).
valor(carta(5-espadas), 2).
valor(carta(6-espadas), 3).
valor(carta(7-espadas), 4).
valor(carta(q-espadas), 5).
valor(carta(j-espadas), 6).
valor(carta(k-espadas), 7).

valor(carta(2-espadas), 9).
valor(carta(3-espadas), 10).


valor(carta(4-ouros), 1).
valor(carta(5-ouros), 2).
valor(carta(6-ouros), 3).

valor(carta(q-ouros), 5).
valor(carta(j-ouros), 6).
valor(carta(k-ouros), 7).
valor(carta(a-ouros), 8).
valor(carta(2-ouros), 9).
valor(carta(3-ouros), 10).

valor(carta(7-ouros), 11).
valor(carta(a-espadas), 12).
valor(carta(7-copas), 13).
valor(carta(4, paus), 14).

posicao(carta(4-paus), 1).
posicao(carta(5-paus), 2).
posicao(carta(6-paus), 3).
posicao(carta(7-paus), 4).
posicao(carta(q-paus), 5).
posicao(carta(j-paus), 6).
posicao(carta(k-paus), 7).
posicao(carta(a-paus), 8).
posicao(carta(2-paus), 9).
posicao(carta(3-paus), 10).

posicao(carta(4-copas), 11).
posicao(carta(5-copas), 12).
posicao(carta(6-copas), 13).
posicao(carta(7-copas), 14).
posicao(carta(q-copas), 15).
posicao(carta(j-copas), 16).
posicao(carta(k-copas), 17).
posicao(carta(a-copas), 18).
posicao(carta(2-copas), 19).
posicao(carta(3-copas), 20).

posicao(carta(4-espadas), 21).
posicao(carta(5-espadas), 22).
posicao(carta(6-espadas), 23).
posicao(carta(7-espadas), 24).
posicao(carta(q-espadas), 25).
posicao(carta(j-espadas), 26).
posicao(carta(k-espadas), 27).
posicao(carta(a-espadas), 28).
posicao(carta(2-espadas), 29).
posicao(carta(3-espadas), 30).

posicao(carta(4-ouros), 31).
posicao(carta(5-ouros), 32).
posicao(carta(6-ouros), 33).
posicao(carta(7-ouros), 34).
posicao(carta(q-ouros), 35).
posicao(carta(j-ouros), 36).
posicao(carta(k-ouros), 37).
posicao(carta(a-ouros), 38).
posicao(carta(2-ouros), 39).
posicao(carta(3-ouros), 40).

carta_print(carta(4-paus),'🃔').
carta_print(carta(5-paus),'🃕').
carta_print(carta(6-paus),'🃖').
carta_print(carta(7-paus),'🃗').
carta_print(carta(q-paus),'🃝').
carta_print(carta(j-paus),'🃛').
carta_print(carta(k-paus),'🃞').
carta_print(carta(a-paus),'🃑').
carta_print(carta(2-paus),'🃒').
carta_print(carta(3-paus),'🃓').

carta_print(carta(4-copas),'🂴').
carta_print(carta(5-copas),'🂵').
carta_print(carta(6-copas),'🂶').
carta_print(carta(7-copas),'🂷').
carta_print(carta(q-copas),'🂽').
carta_print(carta(j-copas),'🂻').
carta_print(carta(k-copas),'🂾').
carta_print(carta(a-copas),'🂡').
carta_print(carta(2-copas),'🂢').
carta_print(carta(3-copas),'🂣').

carta_print(carta(4-ouros),'🃄').
carta_print(carta(5-ouros),'🃅').
carta_print(carta(6-ouros),'🃆').
carta_print(carta(7-ouros),'🃇').
carta_print(carta(q-ouros),'🃍').
carta_print(carta(j-ouros),'🃋').
carta_print(carta(k-ouros),'🃎').
carta_print(carta(a-ouros),'🃁').
carta_print(carta(2-ouros),'🃂').
carta_print(carta(3-ouros),'🃃').

carta_print(carta(4-espadas),'🂤').
carta_print(carta(5-espadas),'🂥').
carta_print(carta(6-espadas),'🂦').
carta_print(carta(7-espadas),'🂧').
carta_print(carta(q-espadas),'🂭').
carta_print(carta(j-espadas),'🂫').
carta_print(carta(k-espadas),'🂮').
carta_print(carta(a-espadas),'🂡').
carta_print(carta(2-espadas),'🂢').
carta_print(carta(3-espadas),'🂣').





baralho([carta(4-paus), carta(5-paus), carta(6-paus), carta(7-paus), carta(q-paus), carta(j-paus), carta(k-paus), carta(a-paus), carta(2-paus), carta(3-paus), carta(4-copas), carta(5-copas), carta(6-copas), carta(7-copas), carta(q-copas), carta(j-copas), carta(k-copas), carta(a-copas), carta(2-copas), carta(3-copas), carta(4-espadas), carta(5-espadas), carta(6-espadas), carta(7-espadas), carta(q-espadas), carta(j-espadas), carta(k-espadas), carta(a-espadas), carta(2-espadas), carta(3-espadas), carta(4-ouros), carta(5-ouros), carta(6-ouros), carta(7-ouros), carta(q-ouros), carta(j-ouros), carta(k-ouros), carta(a-ouros), carta(2-ouros), carta(3-ouros)]).
baralho_buff([4-paus, 5-paus, 6-paus, 7-paus, q-paus, j-paus, k-paus, a-paus, 2-paus, 3-paus, 4-copas, 5-copas, 6-copas, 7-copas, q-copas, j-copas, k-copas, a-copas, 2-copas, 3-copas, 4-espadas, 5-espadas, 6-espadas, 7-espadas, q-espadas, j-espadas, k-espadas, a-espadas, 2-espadas, 3-espadas, 4-ouros, 5-ouros, 6-ouros, 7-ouros, q-ouros, j-ouros, k-ouros, a-ouros, 2-ouros, 3-ouros]).
cls :-
    write('\033\[2J').

%regras
baralho_1([]).
baralho_2([]).


clean_list():-
    retract(baralho_1(_)),
    retract(baralho_2(_)),
    assert(baralho_1([])),
    assert(baralho_2([])).

dar_cartas(CONTROLE):-
    baralho_1(LISTA),
    baralho_buff(LISTA_BARALHO),
    length(LISTA_BARALHO,TAMANHO_1),
    (
    CONTROLE == 1->(
        clean_list()
        );
        cls
    ),
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
        dar_cartas(CONTROLE1)
    );
    CONTROLE >= 3 ->(
        cls
    )
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%JOGADORES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jogar_1(CARTA_JOGADA):-
    cls(),
    mostrar_cartas(1),
    baralho_1(LISTA),
    write('Digite a carta que deseja jogar: EX(0,1,2) '),nl,
    read(X),
    nth0(X, LISTA, CARTA),
    write(CARTA),nl,
    delete(LISTA, CARTA, LISTA1),
    retract(baralho_1(LISTA)),
    asserta(baralho_1(LISTA1)),
    CARTA_JOGADA = CARTA.

jogar_2(CARTA_JOGADA):-
    cls(),
    mostrar_cartas(2),
    baralho_2(LISTA),
    write('Digite a carta que deseja jogar: EX:(0,1,2) '),nl,
    read(X),
    nth0(X, LISTA, CARTA),
    write(CARTA),nl,
    delete(LISTA, CARTA, LISTA1),
    retract(baralho_2(LISTA)),
    asserta(baralho_2(LISTA1)),
    CARTA_JOGADA = CARTA.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







truco():-
    ponstos_rodada(PONTOS_RODADA),
    (
        PONTOS_RODADA == 2 ->(
            NEW_PONTOSRODADA is PONTOS_RODADA + 2,
            retract(ponstos_rodada( PONTOS_RODADA)),
            assert(ponstos_rodada( NEW_PONTOSRODADA))
        );
        PONTOS_RODADA == 4 ->(
            NEW_PONTOSRODADA is PONTOS_RODADA + 2,
            retract(ponstos_rodada( PONTOS_RODADA)),
            assert(ponstos_rodada( NEW_PONTOSRODADA))
        );
        PONTOS_RODADA == 6 ->(
            NEW_PONTOSRODADA is PONTOS_RODADA + 4,
            retract(ponstos_rodada( PONTOS_RODADA)),
            assert(ponstos_rodada( NEW_PONTOSRODADA))
        );
        PONTOS_RODADA == 10 ->(
            NEW_PONTOSRODADA is PONTOS_RODADA + 2,
            retract(ponstos_rodada( PONTOS_RODADA)),
            assert(ponstos_rodada( NEW_PONTOSRODADA))
        );
        nl
    ).

    


%regras


zerar_pontos_rodada():-
    retract(pontos_mao(jogador(1), _)),
    retract(pontos_mao(jogador(2), _)),
    assert(pontos_mao(jogador(1), 0)),
    assert(pontos_mao(jogador(2), 0)),
    retract(mao(_)),
    assert(mao(1)),
    retract(ponstos_rodada(_)),
    assert(ponstos_rodada(2)).



desistir(JOGADOR):-
    ponstos_rodada(PONTOS_RODADA),
    pontos(jogador(JOGADOR), PONTOS_MAO),
    NEW_PONTOS_MAO is PONTOS_MAO + PONTOS_RODADA,
    retract(pontos(jogador(JOGADOR), PONTOS_MAO)),
    assert(pontos(jogador(JOGADOR),NEW_PONTOS_MAO)),
    verificar_ganhador_partida(),
    iniciar().




%%%%%%%%%%%%%%%%%%%%%%%%%5%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%VEREFICACAO DE JOGADA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


verificar_ganhador():-
    ponstos_rodada(PONTOS_RODADA),
    mao(Num_mao),
    pontos_mao(jogador(1),PONTOS_MAO_J1),
    pontos_mao(jogador(2),PONTOS_MAO_J2),
    pontos(jogador(1),PONTOS1),
    pontos(jogador(2),PONTOS2),
    (
        PONTOS_MAO_J1>PONTOS_MAO_J2, Num_mao > 2 -> (
            write('           O jogador 1 ganhou a rodada!'),nl,
            nl,
            write('        ###################################'),nl,
            NEW_PONTOS1 is PONTOS1 + PONTOS_RODADA,
            retract(pontos(jogador(1),PONTOS1)),
            asserta(pontos(jogador(1),NEW_PONTOS1)),
            verificar_ganhador_partida(),
            write('digite qualquer coisa para continuar'),nl,
            read(_),
            iniciar()
        );
        PONTOS_MAO_J1<PONTOS_MAO_J2, Num_mao > 2 -> (
            write('          O jogador 2 ganhou a rodada!'),nl,
            nl,
            write('        ###################################'),nl,
            NEW_PONTOS2 is PONTOS2 + PONTOS_RODADA,
            retract(pontos(jogador(2),PONTOS2)),
            asserta(pontos(jogador(2),NEW_PONTOS2)),
            verificar_ganhador_partida(),
            write('digite qualquer coisa para continuar'),nl,
            read(_),
            iniciar()
        );
        PONTOS_MAO_J1 == 2 -> (
            write('          O jogador 1 ganhou a rodada!'),nl,
            nl,
            write('        ###################################'),nl,
            NEW_PONTOS1 is PONTOS1 + PONTOS_RODADA,
            retract(pontos(jogador(1),PONTOS1)),
            asserta(pontos(jogador(1),NEW_PONTOS1)),
            verificar_ganhador_partida(),

            write('digite qualquer coisa para continuar'),nl,
            read(_),
            iniciar()
        );
        PONTOS_MAO_J2 == 2 -> (
            write('          O jogador 2 ganhou a rodada!'),nl,
            nl,
            write('        ###################################'),nl,
            NEW_PONTOS2 is PONTOS2 + PONTOS_RODADA,
            retract(pontos(jogador(2),PONTOS2)),
            asserta(pontos(jogador(2),NEW_PONTOS2)),
            verificar_ganhador_partida(),
            write('digite qualquer coisa para continuar'),nl,
            read(_),
            iniciar()
        );
        (
            write('          Ninguem ganhou a rodada!'),nl,
            nl,
            write('        ###################################'),nl,
            NEW_MAO is Num_mao + 1,
            retract(mao(Num_mao)),
            asserta(mao(NEW_MAO))
        )
    ).

verificar_ganhador_partida():-
    pontos(jogador(1), PONTOS1),
    pontos(jogador(2), PONTOS2),
    nl,
    write('        ###################################'),nl,
    (
        PONTOS1 >= 12 -> (
            write('          O jogador 1 ganhou a partida!'),nl,
            nl,          
            write('digite qualquer coisa para continuar'),nl,
            read(_),
            fim(_)
        );
        PONTOS2 >= 12 -> (
            write('          O jogador 2 ganhou a partida!'),nl,
            nl,
            write('digite qualquer coisa para continuar'),nl,
            read(_),
            fim(_)
        );
        write('          Ninguem ganhou a partida!'),nl,
        nl
    ),
    write('        ###################################'),nl.
    


rodada(X,Z):-
    cls,
    carta_print(carta(X),CARTA1),
    carta_print(carta(Z),CARTA2),
    write('        ############CARTAS JOGADAS#########'),nl,
    write('          Jogador 1: '),write(CARTA1),nl,
    write('          Jogador 2: '),write(CARTA2),nl,
    write('        ##########RESUTADO DA MAO###########'),nl,
    valor(carta(X),V1),
    valor(carta(Z),V2),
    imprimir_rodada_ganhador(V1,V2).
imprimir_rodada_ganhador(X,Y):- 
    mao(Num_mao),
    pontos_mao(jogador(1),PONTOS_MAO_J1),
    pontos_mao(jogador(2),PONTOS_MAO_J2),
    (
        X>Y, Num_mao =< 3 -> (
        nl,
        write('          O jogador 1 ganhou a mao!'),nl,
        
        NEW_PONTOS_MAO_J1 is PONTOS_MAO_J1 + 1,
        retract(pontos_mao(jogador(1),PONTOS_MAO_J1)),
        asserta(pontos_mao(jogador(1),NEW_PONTOS_MAO_J1))
        );
        X<Y, Num_mao =< 3 -> (
        nl,
        write('          O jogador 2 ganhou a mao!'),nl,
        NEW_PONTOS_MAO_J2 is PONTOS_MAO_J2 + 1,
        retract(pontos_mao(jogador(2),PONTOS_MAO_J2)),
        asserta(pontos_mao(jogador(2),NEW_PONTOS_MAO_J2))
        );
        X==Y, Num_mao =< 3 -> (
        nl,
        write('          EMPATE!'),nl,
        NEW_PONTOS_MAO_J1 is PONTOS_MAO_J1 + 1,
        retract(pontos_mao(jogador(1),PONTOS_MAO_J1)),
        asserta(pontos_mao(jogador(1),NEW_PONTOS_MAO_J1)),
        NEW_PONTOS_MAO_J2 is PONTOS_MAO_J2 + 1,
        retract(pontos_mao(jogador(2),PONTOS_MAO_J2)),
        asserta(pontos_mao(jogador(2),NEW_PONTOS_MAO_J2))
        )
    ),
    nl,
    write('        ######RESULTADO DA PARTIDA#########'),nl,
    nl,
    retract(trucado(_,_)),
    asserta(trucado(0,1)),
    verificar_ganhador(),
    verificar_ganhador_partida(),
    write('Digite qualquer coisa para continuar...'),nl,
    read(_),
    partida().
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%VISUAL%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mostrar_cartas_1(LOOP):-
    baralho_1(LISTA),
    length(LISTA,TAMANHO),  
    (
    LOOP < TAMANHO ->(
        
        nth0(LOOP, LISTA, CARTA),
        carta_print(carta(CARTA),X),
        write(' '), write(X), write(' '),
        LOOP1 is LOOP + 1,
        mostrar_cartas_1(LOOP1)
    );
    LOOP >= TAMANHO ->(
        write(' ')
    )
    ).



mostrar_cartas_2(LOOP):-
        baralho_2(LISTA),
        length(LISTA,TAMANHO),  
        (
        LOOP < TAMANHO ->(
            
            nth0(LOOP, LISTA, CARTA),
            carta_print(carta(CARTA),X),
            write(' '), write(X), write(' '),
            LOOP1 is LOOP + 1,
            mostrar_cartas_2(LOOP1)
        );
        LOOP >= TAMANHO ->(
            write(' ')
        )
        ). 


estado_partida():-
    cls,
    mao(X1),
    pontos(jogador(1), X4),
    pontos(jogador(2), X5),
    pontos_mao(jogador(1), X6),
    pontos_mao(jogador(2), X7),
    ponstos_rodada(X8),
    write('        ##############PONTOS###############'),nl,
    nl,
    write('         O Jogador 1 tem '), write(X4), write(' pontos'), nl,
    write('         O Jogador 2 tem '), write(X5), write(' pontos'), nl,
    nl,
    write('        ##############MAO##################'),nl,
    nl,
    write('         O Jogador 1 tem '), write(X6), write(' mãos ganha'), nl,
    write('         O Jogador 2 tem '), write(X7), write(' mãos ganha'), nl,
    nl,
    write('        #########DADOS DA PARTIDA##########'),nl,
    nl,
    write('         A rodada está valendo  '), write(X8), write(' pontos'), nl,
    write('         Estamos na mão  '), write(X1),nl,
    nl,
    write('        ###################################'),nl.

mostrar_cartas(BARALHO):-
    cls,
    carta_atual_1(Carta_atual_1),
    carta_atual_2(Carta_atual_2),
    valor(carta(Carta_atual_1),V1),
    valor(carta(Carta_atual_2),V2),

    (
        V1 =\= 0, V2 =\= 0 -> (
            carta_print(carta(Carta_atual_1),X1),
            carta_print(carta(Carta_atual_2),X2),
            write('        ###########CARTAS JOGADAS#########'),nl,
            nl,
            write('         Jogador 1: '),write(X1),nl,
            write('         Jogador 2: '),write(X2),nl,
            nl
        );
        V1 =\= 0 -> (
            carta_print(carta(Carta_atual_1),X1),
            write('        ##############CARTAS######################'),nl,
            nl,
            write('         Jogador (1) '),write(X1),nl,
            nl
        );
        V2 =\= 0 -> (
            carta_print(carta(Carta_atual_2),X2),
            write('        ##############CARTAS######################'),nl,
            nl,
            write('         Jogador (2) '),write(X2),nl,
            nl
        );
        (
            nl
        )
    ),
    ponstos_rodada(PONTOS),
    write('        ##########################################'),nl,
    nl,
    write('         A partida está valendo : '),write(PONTOS),write(' pontos'),nl,
    nl,
    write('        ##########BARALHO DO JOGADOR('),write(BARALHO),write(')###########'),nl,
    write('         '),nl,
    (
        BARALHO == 1 -> (
            write('                      '), mostrar_cartas_1(0),nl,
            write('         '),nl
        );
        BARALHO == 2 -> (
            write('                      '), mostrar_cartas_2(0),nl,
            write('         '),nl
        )
    ),
    write('        ##########################################'),nl.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%LOOP DE JOGADAS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iniciar():-
    zerar_pontos_rodada(),
    clean_list(),
    dar_cartas(1),
    partida().



partida() :- 
    trucado(ESTADO_TRUCO,PODE_TRUCAR),
    ( ESTADO_TRUCO == 0, PODE_TRUCAR =\= 0 -> (
        estado_partida(),
        write('Digite qualquer coisa para continuar ...'),nl,
        read(_),
        cls,
        write('É a vez do jogador 1!'),nl,
        mostrar_cartas(1)  
        );
        nl
    ),
    (   
        ESTADO_TRUCO == 0,PODE_TRUCAR == 1 -> (
            write('deseja pedir truco ? (0 = não) (1= sim)'),nl,
            read(X),
            (
                X == 1 -> (
                    write('O jogador 1 pediu truco!'),nl,
                    truco(),
                    retract(trucado(_,_)),
                    asserta(trucado(1,0))

                    
                );
                X == 0 -> (
                    write('O jogador 1 não pediu truco!'),nl,
                    jogar_1(Carta1),
                    retract(carta_atual_1(_)),
                    asserta(carta_atual_1(Carta1))

                )
            )
        );
        ESTADO_TRUCO == 1 -> (
            mostrar_cartas(1),
            write('O jogador 2 pediu truco ! (0 = desistir) (1= aceitar) (2 = aumentar)'),nl,
            read(ESCOLHA_TRUCADA2),
            (
                ESCOLHA_TRUCADA2 == 1 -> (
                    write('O jogador 1 aceitou o truco!'),nl,
                    retract(trucado(ESTADO_TRUCO,PODE_TRUCAR)),
                    asserta(trucado(0,0))
                );
                ESCOLHA_TRUCADA2 == 2 -> (
                    write('O jogador 1 aumentou as apostas!'),nl,
                    truco(),
                    retract(trucado(ESTADO_TRUCO,PODE_TRUCAR)),
                    asserta(trucado(1,0))            
                );
                ESCOLHA_TRUCADA2 == 0 -> (
                    write('O jogador 1 desistiu!'),nl,
                    retract(trucado(ESTADO_TRUCO,PODE_TRUCAR)),
                    asserta(trucado(0,1)),
                    desistir(2),
                    partida()
                )
            )
        );
        ESTADO_TRUCO == 0, PODE_TRUCAR == 0 ->(
            jogar_1(Carta1),
            retract(carta_atual_1(_)),
            asserta(carta_atual_1(Carta1)) 
        )
    ),
    
    cls(),
    write('É a vez do jogador 2!'),nl,
    mostrar_cartas(2),
    trucado(ESTADO_TRUCO2,PODE_TRUCAR2),
    (
        ESTADO_TRUCO2 == 0,PODE_TRUCAR2 == 1 -> (
            write('deseja pedir truco ? (0 = não) (1= sim)'),nl,
            read(Y),
            (
                Y == 1 -> (
                    write('O jogador 2 pediu truco!'),nl,
                    truco(),
                    retract(trucado(ESTADO_TRUCO2,PODE_TRUCAR2)),
                    asserta(trucado(1,0)),
                    partida()
                    
                );
                Y == 0 -> (
                    write('O jogador 2 não pediu truco!'),nl,
                    jogar_2(Carta2),
                    retract(carta_atual_2(_)),
                    asserta(carta_atual_2(Carta2))
                    
                )
            )
        );
        
        ESTADO_TRUCO2 == 1 -> (
            write('O jogador 1 pediu truco !(0 = desistir) (1= aceitar) (2 = aumentar)'),nl,
            read(ESCOLHA_TRUCADA),
            (
                ESCOLHA_TRUCADA == 1 -> (
                    write('O jogador 2 aceitou o truco!'),nl,
                    retract(trucado(_,_)),
                    asserta(trucado(0,0)),
                    partida()
                );
                ESCOLHA_TRUCADA == 2 -> (
                    write('O jogador 2 aumentou as apostas!'),nl,
                    truco(),
                    retract(trucado(ESTADO_TRUCO2,PODE_TRUCAR2)),
                    asserta(trucado(1,0)),
                    cls,
                    partida()
                );
                ESCOLHA_TRUCADA == 0 -> (
                    write('O jogador 2 desistiu!'),nl,
                    retract(trucado(ESTADO_TRUCO2,PODE_TRUCAR2)),
                    asserta(trucado(0,1)),
                    desistir(1)
                )
            )
        );
        ESTADO_TRUCO2 == 0, PODE_TRUCAR2 == 0 ->(
            jogar_2(Carta2),
            retract(carta_atual_2(_)),
            asserta(carta_atual_2(Carta2))
        )
        
    ),
    nl,

    carta_atual_1(Carta_atual_1),
    carta_atual_2(Carta_atual_2),
    retract(carta_atual_1(_)),
    retract(carta_atual_2(_)),
    asserta(carta_atual_1(zero)),
    asserta(carta_atual_2(zero)),
    rodada(Carta_atual_1,Carta_atual_2).    
fim(_):-
    cls,
    write('Obrigado por jogar! :) '),nl,
    halt.
