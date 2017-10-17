% [ PROJ1-01 ] Carregue para memória a BC definida no ficheiro
% paises.txt.
:- consult('paises.txt').
:- dynamic cor/2.

%:- dinamic_allocation
% [PROJ1 - 02] Escreva o predicado lista(C) que lista todos os paíse% s
% de um dado continente apresentando em
% cabeçalho o continente e linha a linha os países: <nome>, <população>,
% <lista países fronteira>.

% Cria lista com paises de um dado continente
paisesCont(C,L):- findall(P, pais(P,C, _), L).

% Cria lista com os paises vizinhos de um dado pais
vizinho(P1, P2):- P1\==P2,(fronteira(P1, P2); fronteira(P2, P1)).
vizinhos(P, L) :-  findall(P2, vizinho(P,P2), L).

% Escreve informações sobre cada pais de uma dada lista de paises
sublista([]).
sublista([P|T]):- pais(P,_,PL), vizinhos(P, L), write(P),write(', '), write(PL), write(', '), write(L), nl, sublista(T).


% Escreve informações sobre um dado continente
lista(C):- continente(C), write('Continente: '), write(C), nl, write('---------------------------'), nl, paisesCont(C, L), sublista(L).


% [PROJ1 - 03] Escreva o predicado doisMaisPop(P1, P2) que apresenta os
% dois países com mais habitantes.

% Compara dois paises e retorna o que tiver maior população
maxPop(P1,P2, P3):- pais(P1,_,PL1), pais(P2,_,PL2), PL1 >= PL2,!, P3 = P1.
maxPop(_,P2,P2).

% Retorna o pais com maior população de uma lista
maisPop([],M,M).
maisPop([P|T],M0,M):- maxPop(M0,P,M1), maisPop(T,M1,M).
maisPop(L,M):- [P|T] = L, maisPop(T,P,M).

% Apaga a primeira instancia de um elemento numa lista
del(X, [X|T], T).
del(X, [H|T],[H|L]):- del(X,T,L).

% Organiza todos os paises numa lista, encontra o pais com maior
% população, retira esse pais da lista e repete o processo mais uma vez.
% Isto permite obter os dois paises com maior população.
doisMaisPop(P1,P2):-  findall(P, pais(P,_,_),L), write(L), maisPop(L,P1), del(P1,L,LR), maisPop(LR,P2),!.


% [PROJ1-04] - Escreva o predicado paisesGrandes(C,N,L) que calcula a
% lista de países com mais de N milhões de habitantes de um dado
% continente, ordenada por ordem crescente de população no formato
% indicado:
%    ?- paisesGrandes(europa,50,L).
%    L = [60.59-italia, ..., 79.81-turquia, 82.8-alemanha,
%    146.5-russia].

paisesGrandes(C,N,L):- setof(Pl-P,(pais(P,C,Pl), Pl > N),L).

% [PROJ1 - 05] Escreva  o predicado somaPopViz(P,L,S) que coloca em L os
% pares (População, País-vizinho) e calcula a soma da população de todos os vizinhos do país P.
% Exemplo: somaPopViz(italia,L,S).
% L = [(8.42, suica), (8.77, austria),(2.06, eslovenia), (66.99,franca)]
% S = 86.24 ;

somaPop([],0.0).
somaPop([H|T],S):-somaPop(T,S1), pais(H,_,PL), S is S1+PL.
somaPopViz(P,S):- vizinhos(P,L),somaPop(L,S),!.
somaPopViz(P,L,S):- findall((PL, P2),(vizinho(P, P2), pais(P2,_,PL)), L),somaPopViz(P,S).

% [PROJ1 - 6] Escreva o predicado numPaisesAtravessados(P1, P2, Num) que calcula o menor número de países que é
% necessário atravessar para chegar de P1 a P2.

contar([],0).
contar([_|T],N):- contar(T,N1), N is N1+1.

% Pesquisa em Largura:
npa(Orig, Dest,Num):-
    npa2(Dest,[[Orig]],Num).
% Condição final: destino = nó à cabeça do caminho actual
npa2(Dest, [[Dest|T]|_],Num):-
    % Contar a lista e retirar origem e destino.
    contar([Dest|T],Num1), Num is Num1 - 2.
npa2(Dest, [LA|Outros],Num):-
    LA = [Act|_],
    % Calcular todos os nós adjacentes não visitado e
    % gerar um caminhos novo c/ cada nó e caminho actual
    findall([X|LA],
            (Dest\==Act, vizinho(Act, X), \+ member(X,LA)),
            Novos),
    % Novos caminhos são colocados no final da lista
    % P/ posterior exploração
    append(Outros, Novos, Todos),
    % Chamada recursiva
    npa2(Dest, Todos,Num).

numPaisesAtravessados(P1, P2, Num):-
    pais(P1, C, _),
    pais(P2, C, _), % Verifica se pertence ao mesmo continente.
    npa(P1, P2, Num).

% [PROJ1-07] Dado um país origem, um país destino e o número de fronteiras a atravessar, pretende-se
% que apresentem todos os roteiros possíveis entre esses dois países.

% ### Pesquisa em Largura: ###
bfs(Orig, Dest, Cam) :-
    bfs2(Dest,[[Orig]], Cam).
%condição final: destino = nó à cabeça do caminho actual
bfs2(Dest, [[Dest|T]|_], Cam) :-
    %caminho actual está invertido
    reverse([Dest|T], Cam).
bfs2(Dest, [LA|Outros], Cam) :-
    LA = [Act|_] ,
    %calcular todos os nós adjacentes não visitado e
    %gerar um caminhos novo c/ cada nó e caminho actual
    findall([X|LA],
            (Dest\==Act, vizinho(Act, X), \+ member(X,LA)),
            Novos) ,
    %novos caminhos são colocados no final da lista
    %p/ posterior exploração
    append(Outros, Novos, Todos) ,
    %chamada recursiva
    bfs2(Dest, Todos, Cam).

roteiros(Orig,Dest,N,L):- Orig\==Dest, findall(Cam,(bfs(Orig,Dest,Cam), contar(Cam,F), N1 is N+1, F==N1) , L).

% [PROJ1 - 08] Dado um continente, um pa�s origem e o n�mero de fronteiras a atravessar, apresente todos os roteiros poss�veis.
%

% imprimir lista de listas
%
imprime([]).
imprime([H|T]):- write(H), write('\n'), imprime(T).

% roteiros (com base no bfs)
roteirosSemDest(Orig, Continente, F) :-
    pais(Orig,Continente,_),
    findall(Cam, (dfs(Orig, F, Cam), contar(Cam,Num), Num1 is Num -1, Num1==F), L), imprime(L).

% ### Pesquisa em profundidade: ###
dfs(Orig,F, Cam) :-
    dfs2(Orig, F, [Orig], Cam) .
%condi��o final: nodo actual = destino
dfs2(_, 0, LA, Cam) :-
    reverse(LA, Cam).
dfs2(Act, F, LA, Cam) :-
    %testar liga��o entre ponto
    %actual e um qualquer X
    vizinho(Act, X) ,
    %testar n�o circularidade p/ n�o
    %visitar nodos j� visitados
    \+ member(X,LA) ,
    F1 is F - 1,
    %chamada recursiva
    dfs2(X, F1, [X|LA], Cam) .


% [PROJ1 - 09] - Elabore	o	predicado	colorir_mapa(C)	que	para	os	pa�ses	e	as	fronteiras	carregados	na	BC	relativos	ao	continente	C,	produz	factos	do	tipo	cor(Cor,Pa�s),	de	forma	a	que	pa�ses	vizinhos	n�o	partilhem	a	mesma	cor	e	usando	o	menor	n�mero	poss�vel	de	cores.
%


grau(P,N):- vizinhos(P,L), contar(L,N).
maxGrau(P1,P2,P3):- grau(P1,N1), grau(P2,N2), N1>=N2, !, P3 = P1.
maxGrau(_,P2,P2).

maiorGrau([],M,M).
maiorGrau([P|T],M0,M):- maxGrau(M0,P,M1), maiorGrau(T,M1,M).
maiorGrau(L,M):- [P|T] = L, maiorGrau(T,P,M).


colorir_pais(P,C):- findall(PV, (cor(C,PV),vizinho(P,PV)), Coloridos), contar(Coloridos, N), N==0, assertz(cor(C,P)),write(cor(C,P)), write("\n"),!.
colorir_pais(P,C):- C1 is C+1,colorir_pais(P,C1).

colorir([]).
colorir(L):- maiorGrau(L, P), colorir_pais(P,1), del(P,L,L1), colorir(L1).
colorir_mapa(C):- continente(C), paisesCont(C, L),colorir(L),!.

apagar_cores_c(C):- continente(C), paisesCont(C, L), apagar_cores(L).
apagar_cores([]).
apagar_cores([H|T]):- retractall(cor(_,H)), apagar_cores(T).

%[ PROJ1-10 ] - Crie	o	predicado	checkCores(R)	que	produz	em	R	uma	lista	de	triplos	(P,C,L)	em	que	L	�	o	resultado	da	interse��o	da	cor	C,	do	pa�s	P	com	as	cores	dos	pa�ses	que	fazem	fronteira	com	P.

checkCores(R):- findall((P,C,L),(cor(C,P), findall(PV, (cor(C,PV),vizinho(P,PV)),L)),R), imprime(R).


% 11. Guarde a BC num ficheiro de texto no final do ficheiro
save():-tell('save.txt'), listing(_), told.
