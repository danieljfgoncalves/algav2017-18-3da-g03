continente(africa).
continente(americasul).
continente(asia).
continente(europa).
continente(oceania).

%pais(pais,continente,Milhoes Habitantes)
pais(argentina, americasul, 41.67).
pais(bolivia, americasul, 9.70).
pais(brasil, americasul, 206.12).
pais(chile, americasul, 16.80).
pais(colombia, americasul, 46.86).
pais(equador, americasul, 14.88).
pais(guiana, americasul, 0.07).
pais(guianafrancesa, americasul, 2.88).
pais(paraguai, americasul, 6.24).
pais(peru, americasul, 28.22).
pais(suriname, americasul, 0.04).
pais(venezuela, americasul, 31.02).
pais(uruguai, americasul, 3.35).
pais(albania, europa, 2.88).
pais(alemanha, europa, 82.8).
pais(armenia, europa, 3.01).
pais(austria, europa, 8.77).
pais(belgica, europa, 11.37).
pais(bielorussia, europa, 9.48).
pais(bosnia, europa, 3.75).
pais(bulgaria, europa, 7.1).
pais(chipre, europa, 0.85).
pais(croacia, europa, 4.15).
pais(dinamarca, europa, 5.75).
pais(eslovaquia, europa, 5.44).
pais(eslovenia, europa, 2.06).
pais(espanha, europa, 46.53).
pais(estonia, europa, 1.32).
pais(finlandia, europa, 5.5).
pais(franca, europa, 66.99).
pais(georgia, europa, 3.71).
pais(grecia, europa, 10.76).
pais(holanda, europa, 17.08).
pais(hungria, europa, 9.8).
pais(irlanda, europa, 4.77).
pais(islandia, europa, 0.34).
pais(italia, europa, 60.59).
pais(kosovo, europa, 1.77).
pais(letonia, europa, 1.98).
pais(liechtenstein, europa, 0.04).
pais(lituania, europa, 2.85).
pais(luxemburgo, europa, 0.59).
pais(macedonia, europa, 2.07).
pais(malta, europa, 0.44).
pais(moldavia, europa, 3.55).
pais(monaco, europa, 0.04).
pais(montenegro, europa, 0.62).
pais(noruega, europa, 5.26).
pais(polonia, europa, 38.42).
pais(portugal, europa, 10.31).
pais(reinounido, europa, 65.81).
pais(republicacheca, europa, 10.57).
pais(romenia, europa, 19.64).
pais(russia, europa, 146.5).
pais(servia, europa, 7.04).
pais(suecia, europa, 10).
pais(suica, europa, 8.42).
pais(turquia, europa, 79.81).
pais(ucrania, europa, 42.59).

fronteira(argentina, bolivia).
fronteira(argentina, brasil).
fronteira(argentina, chile).
fronteira(argentina, paraguai).
fronteira(argentina, uruguai).

fronteira(bolivia, brasil).
fronteira(bolivia, chile).
fronteira(bolivia, paraguai).
fronteira(bolivia, peru).

fronteira(brasil, colombia).
fronteira(brasil, guiana).
fronteira(brasil, guianafrancesa).
fronteira(brasil, paraguai).
fronteira(brasil, peru).
fronteira(brasil, suriname).
fronteira(brasil, uruguai).
fronteira(brasil, venezuela).

fronteira(chile, peru).

fronteira(colombia, equador).
fronteira(colombia, peru).
fronteira(colombia, venezuela).

fronteira(equador, peru).

fronteira(guiana, suriname).
fronteira(guiana, venezuela).

fronteira(guianafrancesa, suriname).

fronteira(albania, grecia).
fronteira(albania, macedonia).
fronteira(albania, montenegro).
fronteira(albania, kosovo).

fronteira(alemanha, austria).
fronteira(alemanha, belgica).
fronteira(alemanha, dinamarca).
fronteira(alemanha, franca).
fronteira(alemanha, holanda).
fronteira(alemanha, luxemburgo).
fronteira(alemanha, polonia).
fronteira(alemanha, republicacheca).
fronteira(alemanha, suica).

fronteira(armenia, georgia).
fronteira(armenia, turquia).

fronteira(austria, eslovaquia).
fronteira(austria, eslovenia).
fronteira(austria, hungria).
fronteira(austria, italia).
fronteira(austria, liechtenstein).
fronteira(austria, republicacheca).
fronteira(austria, suica).

fronteira(belgica, franca).
fronteira(belgica, holanda).
fronteira(belgica, luxemburgo).

fronteira(bielorussia, letonia).
fronteira(bielorussia, lituania).
fronteira(bielorussia, polonia).
fronteira(bielorussia, russia).
fronteira(bielorussia, ucrania).

fronteira(bosnia, croacia).
fronteira(bosnia, montenegro).
fronteira(bosnia, servia).

fronteira(bulgaria, grecia).
fronteira(bulgaria, macedonia).
fronteira(bulgaria, romenia).
fronteira(bulgaria, servia).
fronteira(bulgaria, turquia).

fronteira(croacia, eslovenia).
fronteira(croacia, hungria).
fronteira(croacia, montenegro).
fronteira(croacia, servia).

fronteira(eslovaquia, hungria).
fronteira(eslovaquia, polonia).
fronteira(eslovaquia, republicacheca).
fronteira(eslovaquia, ucrania).

fronteira(eslovenia, hungria).
fronteira(eslovenia, italia).

fronteira(espanha, franca).
fronteira(espanha, portugal).

fronteira(estonia, letonia).
fronteira(estonia, russia).

fronteira(finlandia, noruega).
fronteira(finlandia, russia).
fronteira(finlandia, suecia).

fronteira(franca, italia).
fronteira(franca, luxemburgo).
fronteira(franca, monaco).
fronteira(franca, suica).

fronteira(georgia, russia).
fronteira(georgia, turquia).

fronteira(grecia, macedonia).
fronteira(grecia, turquia).

fronteira(hungria, romenia).
fronteira(hungria, servia).
fronteira(hungria, ucrania).

fronteira(irlanda, reinounido).

fronteira(italia, suica).

fronteira(kosovo, macedonia).
fronteira(kosovo, montenegro).
fronteira(kosovo, servia).

fronteira(letonia, lituania).
fronteira(letonia, russia).

fronteira(liechtenstein, suica).

fronteira(lituania, polonia).
fronteira(lituania, russia).

fronteira(macedonia, servia).

fronteira(moldavia, romenia).
fronteira(moldavia, ucrania).

fronteira(noruega, suecia).
fronteira(noruega, russia).

fronteira(montenegro, servia).

fronteira(polonia, republicacheca).
fronteira(polonia, russia).
fronteira(polonia, ucrania).

fronteira(romenia, servia).
fronteira(romenia, ucrania).

fronteira(russia, ucrania).

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
delete(X, [X|T], T).
delete(X, [H|T],[H|L]):- delete(X,T,L).

% Organiza todos os paises numa lista, encontra o pais com maior
% população, retira esse pais da lista e repete o processo mais uma vez.
% Isto permite obter os dois paises com maior população.
doisMaisPop(P1,P2):-  findall(P, pais(P,_,_),L), write(L), maisPop(L,P1), delete(P1,L,LR), maisPop(LR,P2),!.


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


contar([],0).
contar([_|T],N):- contar(T,N1), N is N1+1.

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










