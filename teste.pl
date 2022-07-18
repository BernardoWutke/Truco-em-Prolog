:- dynamic soma/1.


soma(0).

somar(X):- soma(Y), Y1 is Y+X, retract(soma(Y)), asserta(soma(Y1)).


