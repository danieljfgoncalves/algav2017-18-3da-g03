% display Knowledge Base

% Boards intial state [B]

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
