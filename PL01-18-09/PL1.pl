continente(europa).
continente(africa).
continente(america).
continente(asia).
continente(oceania).

pais(portugal,europa,10).
pais(espanha,europa,20).
pais(usa,america,100).
pais(mexico,america,100).
pais(belgica, europa, 10).
pais(franca, europa, 30).
pais(inglaterra, europa, 15).

fronteira(portugal,espanha).
fronteira(espanha,franca).
fronteira(franca,belgica).
fronteira(usa,mexico).

populacao(X):- pais(X,_,Y), write(X), write(' tem '), write(Y), write(' milhoes de pessoas').

vizinho(P1, P2):- P1\==P2,(fronteira(P1, P2); fronteira(P2, P1)).

contSemPaises(C) :- continente(C), not(pais(_,C,_)).

semVizinhos(L) :- vizinho(L,_),!,fail.
semVizinhos(_).

chegoLaFacil(P1,P2):- vizinho(P1,P2); (vizinho(P1,P3), vizinho(P3, P2)).

pot(_,0,1):-!.
pot(B,E,R):- E1 is E-1, pot(B,E1,R1), R is B*R1.


fat(1,1):-!.
fat(N,F):- N1 is N-1,fat(N1,F1), F is N * F1.

somatorio(A,A,A).
somatorio(A,B, R):-  A>B, A1 is A-1, somatorio(A1,B, R1), R is A+R1,!.
somatorio(A,B, R):-  B>A, B1 is B-1, somatorio(B1,A, R1), R is B+R1,!.

idiv(A,B):- Q is div(A,B),R is mod(A,B), write('Quociente: '), write(Q), nl, write('Resto: '), write(R).

primo(N):- N=<1,!,fail.
primo(2):-!.
primo(3):-!.
primo(5):-!.
primo(7):-!.
primo(9):-!.
primo(N):- R is mod(N,2), R==0,!,fail.
primo(N):- R is mod(N,3), R==0,!,fail.
primo(N):- R is mod(N,5), R==0,!,fail.
primo(N):- R is mod(N,7), R==0,!,fail.
primo(N):- R is mod(N,9), R==0,!,fail.
primo(_).

mdc(A,B):- R is mod(A,B), R==0, write(B), !.
mdc(A,B):- R is mod(A,B), R\==0, mdc(B,R).
