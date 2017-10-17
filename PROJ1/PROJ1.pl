% [ PROJ1-01 ] Carregue para memória a BC definida no ficheiro
% paises.txt.
:- consult('paises.txt').

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
contar([_|T],N):- cont(T,N1), N is N1+1.

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
