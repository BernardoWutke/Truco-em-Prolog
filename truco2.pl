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





valor(carta(5-paus),2).
valor(carta(6-paus),3).
valor(carta(7-paus),4).
valor(carta(q-paus),5).
valor(carta(j-paus),6).
valor(carta(k-paus),7).
valor(carta(a-paus),8).
valor(carta(2-paus),9).
valor(carta(3-paus),10).

valor(carta(4-copas),1).
valor(carta(5-copas),2).
valor(carta(6-copas),3).

valor(carta(q-copas),5).
valor(carta(j-copas),6).
valor(carta(k-copas),7).
valor(carta(a-copas),8).
valor(carta(2-copas),9).
valor(carta(3-copas),10).

valor(carta(4-espadas),1).
valor(carta(5-espadas),2).
valor(carta(6-espadas),3).
valor(carta(7-espadas),4).
valor(carta(q-espadas),5).
valor(carta(j-espadas),6).
valor(carta(k-espadas),7).

valor(carta(2-espadas),9).
valor(carta(3-espadas),10).


valor(carta(4-ouros),1).
valor(carta(5-ouros),2).
valor(carta(6-ouros),3).

valor(carta(q-ouros),5).
valor(carta(j-ouros),6).
valor(carta(k-ouros),7).
valor(carta(a-ouros),8).
valor(carta(2-ouros),9).
valor(carta(3-ouros),10).

valor(carta(7-ouros),11).
valor(carta(a-espadas),12).
valor(carta(7-copas),13).
valor(carta(4,paus),14).


truco(PONTOS1,PONTOS2,PONTOS_RODADA,ETAPA):-
    PONTOS_RODADA == 2,
    NEW_PONTOSRODADA is 4,
    start(PONTOS1,PONTOS2,NEW_PONTOSRODADA,ETAPA);
    PONTOS_RODADA == 4,
    NEW_PONTOSRODADA is 6,
    start(PONTOS1,PONTOS2,NEW_PONTOSRODADA,ETAPA);
    PONTOS_RODADA == 6,
    NEW_PONTOSRODADA is 10,
    start(PONTOS1,PONTOS2,NEW_PONTOSRODADA,ETAPA);
    PONTOS_RODADA == 10,
    NEW_PONTOSRODADA is 12,
    start(PONTOS1,PONTOS2,NEW_PONTOSRODADA,ETAPA).
    


%regras
rodada(X,Z,PONTOS1,PONTOS2,PONTOS_RODADA,ETAPA):-
    valor(carta(X),V1),
    valor(carta(Z),V2),
    imprimir_rodada_ganhador(V1,V2,PONTOS1,PONTOS2,PONTOS_RODADA,ETAPA).

imprimir_rodada_ganhador(X,Y,PONTOS1,PONTOS2,PONTOS_RODADA,ETAPA):- 
    X>Y,
    write('O jogador 1 ganhou a rodada!'),nl,
    NEW_PONTOS1 is PONTOS1 + PONTOS_RODADA,
    start(NEW_PONTOS1,PONTOS2,PONTOS_RODADA,ETAPA);
    Y>X,
    write('O jogador 2 ganhou a rodada!'),nl,
    NEW_PONTOS2 is PONTOS2 + PONTOS_RODADA,
    start(PONTOS1,NEW_PONTOS2,PONTOS_RODADA,ETAPA).

    
start(PONTOS1,PONTOS2,PONTOS_RODADA,ETAPA):-
    ETAPA == 1,
    write('Jogador 1 está com '),write(PONTOS1),write(' pontos'),nl,
    write('Jogador 2 está com '),write(PONTOS2),write(' pontos'),nl,
    write('e a vez de jogador1 deseja pedir truco? 0 para sim, 1 para não'),nl,
    read(T),
    T == 0,
    truco(PONTOS1,PONTOS2,NEW_PONTOSRODADA,2);
    T == 1,
    start(PONTOS1,PONTOS2,NEW_PONTOSRODADA,2);
    ETAPA == 2,
    write('Jogador 1, escolha uma carta:'),nl,
    read(CARTA1),
    write('Jogador 2, escolha uma carta:'),nl,
    read(CARTA2),
    rodada(CARTA1,CARTA2,PONTOS1,PONTOS2,PONTOS_RODADA,1).

    
 