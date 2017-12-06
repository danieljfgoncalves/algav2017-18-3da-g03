start():-initialState(B), J='X', game(B,J).

game(board(B),J):-
  display(B),
  readColumn(J,C),
  play(J,C,B,B2), 
  (
    victory(J,B2), display(B2), nl , write('Winner Jogador '), write(J);
    (J='X',J1='Y';J1='X'),game(board(B2),J1)
  ).

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
    board([ ['_','_','_','_','Y','Y'],
            ['X','_','_','X','X','X'],
            ['_','X','_','_','_','_'],
            ['_','_','X','_','_','_'],
            ['_','_','_','X','_','_'],
            ['_','_','_','_','_','_'],
            ['_','_','_','_','_','Y']])
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
  
  draw(B):- 
    \+ (
      append(_,[C|_],B),
      append(_,['_'|_],C)
    ).
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%  COMPLEMENTARY RESEARCH  %%%%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  % [link: http://lpn.swi-prolog.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse24]
  
  % Power of append
  %
  % One important use of append/3 is to split up a list into two consecutive lists. For example:
  %
  % ?- append(X,Y,[a,b,c,d]).
  % X = []
  % Y = [a,b,c,d] ;
  %
  % X = [a]
  % Y = [b,c,d] ;
  %
  % X = [a,b]
  % Y = [c,d] ;
  %
  % X = [a,b,c]
  % Y = [d] ;
  %
  % X = [a,b,c,d]
  % Y = [] ;
  %
  % false
  %
  % That is, we give the list we want to split up (here [a,b,c,d] ) to
  % append/3 as the third argument, and we use variables for the first two
  % arguments. Prolog then searches for ways of instantiating the variables
  % to two lists that concatenate to give the third argument, thus
  % splitting up the list in two. Moreover, as this example shows, by
  % backtracking, Prolog can find all possible ways of splitting up a list
  % into two consecutive lists.
  
  % This ability means it is easy to define some useful predicates with
  % append/3 . Let’s consider some examples. First, we can define a program
  % which finds prefixes of lists. For example, the prefixes of [a,b,c,d]
  % are [] , [a] , [a,b] , [a,b,c] , and [a,b,c,d] . With the help of
  % append/3 it is straightforward to define a program prefix/2 , whose
  % arguments are both lists, such that prefix(P,L) will hold when P is a
  % prefix of L . Here’s how:
  
  % prefix(P,L):- append(P,_,L).
  %
  % X = [] ;
  %
  % X = [a] ;
  %
  % X = [a,b] ;
  %
  % X = [a,b,c] ;
  %
  % X = [a,b,c,d] ;
  %
  % false
