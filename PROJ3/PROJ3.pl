start():-initialState(B), J='o', game(B,J),!.
% start():-initialState(B),board(B1) = B,minimax([x,
% play,B1],N,I,0),write(N), write(I).

goodMove(J,Col,B):-
  maxConnections(J,Col,B,MaxConn),
  MaxConn >= 12.


maxConnections(J,Col,B,I):-
    vertical(J,Col,B,I1),
    horizontal(J,Col,B,I2),!,
    I is I1+I2.


vertical(J,Col,B,I):-
    append(Index,[C|_],B),
    length(Index,Col),
    maxVerticalConnections(J,C,I).

maxVerticalConnections(_,[],0).
maxVerticalConnections(J,['_'|X],N):- maxVerticalConnections(J,X,N1), N is N1+1.
maxVerticalConnections(J,[X|_],0):- X\=J.
maxVerticalConnections(J,[J|X],N):- maxVerticalConnections(J,X,N1), N is N1+1.

horizontal(J,Col,B,I):-
    append(Index,[C|Rest],B),
    length(Index,Col),!,
    (
        append(Aux, [J|_], C);
        append(Aux, ['_'|[]],C);
        append(Aux, ['_'|R],C), R \= ['_'|_]
    ),
        length(Aux,Index2),!,
    write(Index),nl,

    maxHorizontalConnections(J,Index2, Col,Index,Rest,I).

maxHorizontalConnections(J,Index2,Col,Index,Rest,N):-
  maxHorizontalConnectionsLeft(J,Index2,Rest,NL),write(NL),nl,
  maxHorizontalConnectionsRight(J,Index2,Col,Index,NR),write(NR),nl
  , N is NL+NR.

maxHorizontalConnectionsLeft(_,_,[],0).

maxHorizontalConnectionsLeft(J,I,B,N):-
  append(_,[C|Rest], B),
  append(Aux,['_'|_],C),
  length(Aux,I),
  maxHorizontalConnectionsLeft(J,I,Rest,N1),!,
  N is N1+1.

maxHorizontalConnectionsLeft(J,I,B,N):-
  append(_,[C|Rest], B),
  append(Aux,[J|_],C),
  length(Aux,I),
  maxHorizontalConnectionsLeft(J,I,Rest,N1),!,
  N is N1+1.

maxHorizontalConnectionsLeft(J,I,B,N):-
  append(_,[C|_],B),
  append(Aux,[X|_],C),
  length(Aux,I),
  X \= J,!,
  N = 0.


maxHorizontalConnectionsRight(_,_,_,[],0).

maxHorizontalConnectionsRight(J,I,B,N):-
  B = [C|Rest],
  append(Aux,['_'|_],C),
  length(Aux,I),!,
  maxHorizontalConnectionsRight(J,I,Rest,N1),
  N is N1+1.

maxHorizontalConnectionsRight(J,I,B,N):-
  B = [C|Rest],
  append(Aux,[J|_],C),
  length(Aux,I),!,
  maxHorizontalConnectionsRight(J,I,Rest,N1),
  N is N1+1.

maxHorizontalConnectionsRight(J,I,B,N):-
  B = [C|Rest],
  append(Aux,[X|_],C),
  length(Aux,I),
  X \=J,
  maxHorizontalConnectionsRight(J,I,Rest,N1),
  N is N1.

game(board(B),J):-
  display(B),
  readColumn(J,C),
  play(J,C,B,B2),
  (
    victory(J,B2), display(B2), nl , write('Winner Jogador '), write(J);
    (J='x',J1='o';J1='x'),minimax([J1,play,B2],[_,_,B3],_,0),
    (
      victory(J1,B3), display(B3), nl , write('Winner Jogador '), write(J1);
      game(board(B3),J)
    )
  ).

machine(J,_,B,B2):-
  iMachine(J,B,C,B2),
  nl, write('machine: '),
  associateChar(L,C),
  write(L),
  nl,!.

machine(J,O,B,B2):-
  findall((Col,BA), (column(Col), play(J,Col,B,BA),\+ iMachine(O,BA,_,_), goodMove(J,Col,B)), [(C,B2)|_]),
  nl, write('machine: '),
  associateChar(L,C),
  write(L),
  nl,!.


machine(J,O,B,B2):-
  findall((Col,BA), (column(Col), play(J,Col,B,BA),\+ iMachine(O,BA,_,_)), [(C,B2)|_]),
  nl, write('machine: '),
  associateChar(L,C),
  write(L), nl,
  write('-'),!.

machine(J,O,B,B2):-
  iMachine(O,B,C,_),
  play(J,C,B,B2),
  nl, write('machine: '),
  associateChar(L,C),
  write(L), nl.

machine(J,_,B,B2):-
  column(C),
  play(J,C,B,B2),
  nl, write('machine: '),
  associateChar(L,C),
  write(L), nl.

associateChar(L, C):- Ln is 65+C,
atom_codes(L,[Ln]).

iMachine(R,T,C,T2):- findall((Col,TA), (column(Col), play(R,Col,T,TA),victory(R,TA)),[(C,T2)|_]).

%MOVESSS
moves(Pos,NextPosList):-
  findall(NextPos, move(Pos, NextPos), NextPosList).

minimax(Pos, BestNextPos, Val, D) :-
  D<4,
  moves(Pos,NextPosList),
  best(NextPosList, BestNextPos, Val,D), !.

minimax(Pos, _, Val,_) :-
  utility(Pos, Val).

best([Pos], Pos, Val,D) :-
   D1 is D+1,
  minimax(Pos, _, Val,D1), !.

best([Pos1 | PosList], BestPos, BestVal,D) :-
  D1 is D+1,
  minimax(Pos1, _, Val1,D1),
  best(PosList, Pos2, Val2,D),
  betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).


betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :- % Pos0 better than Pos1
min_to_move(Pos0), % MIN to move in Pos0
Val0 > Val1, ! % MAX prefers the greater value
;
max_to_move(Pos0), % MAX to move in Pos0
Val0 < Val1, !. % MIN prefers the lesser value
betterOf(_, _, Pos1, Val1, Pos1, Val1). % Otherwise Pos1 better than Pos0

min_to_move([o, _, _]).
max_to_move([x, _, _]).

move([X1, play, Board], [X2, win, NextBoard]) :-
nextPlayer(X1, X2),
play(X1,_, Board, NextBoard),
victory(X1, NextBoard), !.

move([X1, play, Board], [X2, draw, NextBoard]) :-
nextPlayer(X1, X2),
play(X1,_, Board, NextBoard),
draw(X1,NextBoard), !.

move([X1, play, Board], [X2, play, NextBoard]) :-
nextPlayer(X1, X2),
play(X1,_, Board, NextBoard).


nextPlayer(o, x).
nextPlayer(x, o).
% display Knowledge Base

% Boards intial state [B]
%     L-1 | L-2 | L-3 | L-4 | L-5 | L-6
% C-A
% C-B
% C-C
% C-D
% C-E
% C-F
% C-G
initialState(
    board([ ['_','_','_','_','_','_'],
            ['_','_','_','_','_','_'],
            ['_','_','_','_','_','_'],
            ['_','_','_','_','_','_'],
            ['_','_','_','_','_','_'],
            ['_','_','_','_','_','_'],
            ['_','_','_','_','_','_']])
  ).

  victoryState(
    board([ ['_','_','_','_','_','x'],
            ['_','_','_','_','x','x'],
            ['_','_','_','_','_','x'],
            ['_','_','_','_','_','x'],
            ['_','_','_','_','_','_'],
            ['_','_','_','_','_','_'],
            ['_','_','_','_','_','o']])
  ).

  %%%%%%%%%%%%%%%%%%%%%
  %%%%%  DISPLAY  %%%%%
  %%%%%%%%%%%%%%%%%%%%%

  % Write x number of lines from board
  displayBoard(_, 0).
  displayBoard(B, NumL) :-
    displayLine(B, NumL, Baux),
    NLaux is NumL - 1,
    displayBoard(Baux, NLaux).

  displayLine(B, NumL, Baux) :-
    write(NumL),
    write(' '),
    displayLineAux(B, Baux), nl.

  displayLineAux([], _).
  displayLineAux(B, B2) :-
    B = [[X|RestCol]|NxtColumns],
    B2 = [RestCol|NxtRestColumns],
    write(X),
    write(' '),
    displayLineAux(NxtColumns, NxtRestColumns).


  display(B) :-
    write('  A B C D E F G'), nl,
    displayBoard(B, 6), !.

  %%%%%%%%%%%%%%%%%%%%%
  %%% SELECT COLUMN %%%
  %%%%%%%%%%%%%%%%%%%%%

  % Valid columns
  column(0).
  column(1).
  column(2).
  column(3).
  column(4).
  column(5).
  column(6).

  readColumn(J,Col):-
    write('[Jogador '), write(J), write('] - '),
    write('Select a valid column?'), nl,
    repeat,
    get_char(Input),
    atom_codes(Input, [Char|_]),
    Col is Char-65,
    column(Col).

  %%%%%%%%%%%%%%%%%%%%%
  %%%  GAME LOGIC   %%%
  %%%%%%%%%%%%%%%%%%%%%

  % insertToColumn(X,Col,Col2)
  insertToColumn(X, ['_'], [X]) :- !. % Plays at last spot
  insertToColumn(X, ['_',Y|YS], [X,Y|YS]) :-
    Y \== ('_'), !. % Plays before last piece
  insertToColumn(X,['_'|YS], ['_'|YS2]) :- insertToColumn(X, YS, YS2). % check line below.

  % play is true if B2 is same as board B after X's move to Column C
  play(X,C,B,B2):-
    append(Ind,[Col|Rest],B),
    length(Ind,C),
    insertToColumn(X,Col,Col2),
    append(Ind,[Col2|Rest],B2).


  % Victory
  %
  % Vertical
  victory(X, B):-
    append(_, [Col|_], B),
    append(_, [X,X,X,X|_], Col).
  % Horizontal
  victory(X, B) :-
    append(_,[Col|Rest], B),
    append(_,[X|I], Col),
    length(I,Index),
    Rest = [C2|[C3|[C4|_]]],
    append(_,[X|I2],C2),
    length(I2,Index),
    append(_,[X|I3],C3),
    length(I3,Index),
    append(_,[X|I4],C4),
    length(I4,Index).
  % Diagonal \
  victory(X, B):-
    append(_,[Col|Rest], B),
    append(_,[X|I], Col),
    length(I,Index),
    Rest = [C2|[C3|[C4|_]]],
    append(_,[X|I2],C2),
    Index2 is Index + 1,
    length(I2,Index2),
    append(_,[X|I3],C3),
    Index3 is Index2 + 1,
    length(I3,Index3),
    append(_,[X|I4],C4),
    Index4 is Index3 + 1,
    length(I4,Index4).
  % Diagonal /
  victory(X, B):-
    append(_,[Col|Rest], B),
    append(_,[X|I], Col),
    length(I,Index),
    Rest = [C2|[C3|[C4|_]]],
    append(_,[X|I2],C2),
    length(I2,Index2),
    append(_,[X|I3],C3),
    length(I3,Index3),
    append(_,[X|I4],C4),
    length(I4,Index4),
    Index2 is Index-1, Index3 is Index2-1,Index4 is Index3-1.

  draw(_,B):-
    \+ (
      append(_,[C|_],B),
      append(_,['_'|_],C)
    ).

% utility(+Pos, -Val) :-
% True if Val is the result of the evaluation function at Pos.
% We will only evaluate for final position.
% So we will only have MAX win, MIN win or draw.
% We will use 1 when MAX win
% -1 when MIN win
% 0 otherwise.
utility([o,win, _], 99999). % Previous player (MAX) wins.
utility([x,win, _], -99999). % Previous player (MIN) wins.
utility([_,draw, _],0).
utility([o,play,B],Val):- eval(o,B,Val).
utility([x,play,B],-Val):- eval(x,B,Val).


eval(J,B,Val):-
  count(three(J,B),T),
  count(two(J,B),D),
  Val is ( (T*3) + (D*2) ).



alphabeta(Pos, Alpha, Beta, GoodPos, Val) :-
moves(Pos, PosList), !,
boundedbest(PosList, Alpha, Beta, GoodPos, Val);
utility(Pos, Val).
boundedbest([Pos|PosList], Alpha, Beta, GoodPos, GoodVal) :-
alphabeta(Pos, Alpha, Beta, _, Val),
goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal).
goodenough([], _, _, Pos, Val, Pos, Val) :- !.
goodenough(_, Alpha, Beta, Pos, Val, Pos, Val) :-
min_to_move(Pos), Val > Beta, !
;
max_to_move(Pos), Val < Alpha, !.
goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal) :-
newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta),
boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1),
betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).
newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
min_to_move(Pos), Val > Alpha,!.
newbounds(Alpha, Beta, Pos, Val, Alpha, Val) :-
max_to_move(Pos), Val < Beta, !.
newbounds(Alpha, Beta, _, _, Alpha, Beta).




 % Trios
  %
  % Vertical
  three(X, B):-
    append(_, [Col|_], B),
    append(_, [X,X,X|_], Col).
  % Horizontal
  three(X, B) :-
    append(_,[Col|Rest], B),
    append(_,[X|I], Col),
    length(I,Index),
    Rest = [C2|[C3|_]],
    append(_,[X|I2],C2),
    length(I2,Index),
    append(_,[X|I3],C3),
    length(I3,Index).

    % Diagonal \
  three(X, B):-
    append(_,[Col|Rest], B),
    append(_,[X|I], Col),
    length(I,Index),
    Rest = [C2|[C3|_]],
    append(_,[X|I2],C2),
    Index2 is Index + 1,
    length(I2,Index2),
    append(_,[X|I3],C3),
    Index3 is Index2 + 1,
    length(I3,Index3).


    % Diagonal /
  three(X, B):-
    append(_,[Col|Rest], B),
    append(_,[X|I], Col),
    length(I,Index),
    Rest = [C2|[C3|_]],
    append(_,[X|I2],C2),
    length(I2,Index2),
    append(_,[X|I3],C3),
    length(I3,Index3),
    Index2 is Index-1, Index3 is Index2-1.

% Duos
  %
  % Vertical
  two(X, B):-
    append(_, [Col|_], B),
    append(_, [X,X|_], Col).
  % Horizontal
  two(X, B) :-
    append(_,[Col|Rest], B),
    append(_,[X|I], Col),
    length(I,Index),
    Rest = [C2|_],
    append(_,[X|I2],C2),
    length(I2,Index).

    % Diagonal \
  two(X, B):-
    append(_,[Col|Rest], B),
    append(_,[X|I], Col),
    length(I,Index),
    Rest = [C2|_],
    append(_,[X|I2],C2),
    Index2 is Index + 1,
    length(I2,Index2).


    % Diagonal /
  two(X, B):-
    append(_,[Col|Rest], B),
    append(_,[X|I], Col),
    length(I,Index),
    Rest = [C2|_],
    append(_,[X|I2],C2),
    length(I2,Index2),
    Index2 is Index-1.

count(P,Count) :-
        findall(1,P,L),
        length(L,Count).













