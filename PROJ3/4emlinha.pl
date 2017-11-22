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


display(board(B)) :-
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


read_move(Col):-
  write('Select a valid column?'), nl,
  repeat,
  get_char(Input),
  atom_codes(Input, [Char|_]),
  Col is Char-65,
  column(Col).

  
play:- read_move(Col), write(Col).