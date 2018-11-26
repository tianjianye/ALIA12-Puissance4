:- dynamic board/1.
%value(PlayerFlag,Move,NewBoard,V):-

%-----------------------------------------------ligne horizontale
%colonne 0
positionHorizontale(Player,0,[Player,Player,Player,Player,X|_],1000):-X\==Player.
positionHorizontale(Player,0,[Player,Player,Player,X|_],100):-X\==Player.
positionHorizontale(Player,0,[Player,Player,X|_],10):-X\==Player.
%colonne 1
positionHorizontale(Player,1,[Player,Player,Player,Player,X|_],1000):-X\==Player.
positionHorizontale(Player,1,[Y,Player,Player,Player,Player,X|_],1000):-X\==Player,Y\==Player.
positionHorizontale(Player,1,[Player,Player,Player,X|_],100):-X\==Player.
positionHorizontale(Player,1,[X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionHorizontale(Player,1,[Player,Player,X|_],10):-X\==Player.
positionHorizontale(Player,1,[X,Player,Player,Y|_],10):-X\==Player,Y\==Player.
%colonne2
positionHorizontale(Player,2,[Player,Player,Player,Player,X|_],1000):-X\==Player.
positionHorizontale(Player,2,[Y,Player,Player,Player,Player,X|_],1000):-X\==Player,Y\==Player.
positionHorizontale(Player,2,[_,Y,Player,Player,Player,Player,X|_],1000):-X\==Player,Y\==Player.
positionHorizontale(Player,2,[Player,Player,Player,X|_],100):-X\==Player.
positionHorizontale(Player,2,[_,X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionHorizontale(Player,2,[_,X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionHorizontale(Player,2,[_,Y,Player,Player,X|_],10):-X\==Player,Y\==Player.
positionHorizontale(Player,2,[X,Player,Player,Y|_],10):-X\==Player,Y\==Player.
%colonne3
positionHorizontale(Player,3,[Player,Player,Player,Player,X|_],1000):-X\==Player.
positionHorizontale(Player,3,[Y,Player,Player,Player,Player,X|_],1000):-X\==Player,Y\==Player.
positionHorizontale(Player,3,[_,Y,Player,Player,Player,Player,X|_],1000):-X\==Player,Y\==Player.
positionHorizontale(Player,3,[_,_,Y,Player,Player,Player,Player,X|_],1000):-X\==Player,Y\==Player.
positionHorizontale(Player,3,[Y,Player,Player,Player,X|_],100):-X\==Player,Y\==Player.
positionHorizontale(Player,3,[_,X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionHorizontale(Player,3,[_,_,X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionHorizontale(Player,3,[_,_,Y,Player,Player,X|_],10):-X\==Player,Y\==Player.
positionHorizontale(Player,3,[_,X,Player,Player,Y|_],10):-X\==Player,Y\==Player.
%colonne 4
positionHorizontale(Player,4,[Y,Player,Player,Player,Player,X|_],1000):-X\==Player,Y\==Player.
positionHorizontale(Player,4,[_,Y,Player,Player,Player,Player,X|_],1000):-X\==Player,Y\==Player.
positionHorizontale(Player,4,[_,_,Y,Player,Player,Player,Player,X|_],1000):-X\==Player,Y\==Player.
positionHorizontale(Player,4,[_,X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionHorizontale(Player,4,[_,_,X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionHorizontale(Player,4,[_,_,_,X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionHorizontale(Player,4,[_,_,X,Player,Player,Y|_],10):-X\==Player,Y\==Player.
positionHorizontale(Player,4,[_,_,_,X,Player,Player,Y|_],10):-X\==Player,Y\==Player.
%colonne 5
positionHorizontale(Player,5,[_,X,Player,Player,Player,Player,Y|_],1000):-X\==Player,Y\==Player.
positionHorizontale(Player,5,[_,_,X,Player,Player,Player,Player],1000):-X\==Player.
positionHorizontale(Player,5,[_,_,X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionHorizontale(Player,5,[_,_,_,X,Player,Player,Player],100):-X\==Player.
positionHorizontale(Player,5,[_,_,_,X,Player,Player,Y|_],10):-X\==Player,Y\==Player.
positionHorizontale(Player,5,[_,_,_,_,X,Player,Player],10):-X\==Player.
%colonne 6
positionHorizontale(Player,0,[_,_,X,Player,Player,Player,Player],1000):-X\==Player.
positionHorizontale(Player,0,[_,_,_,X,Player,Player,Player],100):-X\==Player.
positionHorizontale(Player,0,[_,_,_,_,X,Player,Player],10):-X\==Player.
position(Player,Move,NewBoard,V):-.

% Test positionHorizontale
length(Board,7),positionHorizontale(x,6,Board,V).

positionVerticale(Player,[Player,Player,X|_],10):-X\==Player.
positionVerticale(Player,[X,Player,Player,Y|_],10):-X\==Player,Y\==Player.
positionVerticale(Player,[_,X,Player,Player,Y|_],10):-X\==Player,Y\==Player.
positionVerticale(Player,[_,_,X,Player,Player,Y|_],10):-X\==Player,Y\==Player.
positionVerticale(Player,[_,_,_,X,Player,Player|_],10):-X\==Player.

positionVerticale(Player,[Player,Player,Player,X|_],100):-X\==Player.
positionVerticale(Player,[X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionVerticale(Player,[_,X,Player,Player,Player,Y|_],100):-X\==Player,Y\==Player.
positionVerticale(Player,[_,_,X,Player,Player,Player|_],100):-X\==Player.

positionVerticale(Player,[Player,Player,Player,Player,X|_],1000):-X\==Player.
positionVerticale(Player,[X,Player,Player,Player,Player,Y|_],1000):-X\==Player,Y\==Player.
positionVerticale(Player,[_,X,Player,Player,Player,Player|_],1000):-X\==Player.

positionVerticale(Player,0,[X5,_,_,_,_,_,_,
                            X4,_,_,_,_,_,_,
                            X3,_,_,_,_,_,_,
                            X2,_,_,_,_,_,_,
                            X1,_,_,_,_,_,_,
                            X0,_,_,_,_,_,_],V):-positionVerticale(Player,[X0,X1,X2,X3,X4,X5],V).

positionVerticale(Player,1,[_,X5,_,_,_,_,_,
                            _,X4,_,_,_,_,_,
                            _,X3,_,_,_,_,_,
                            _,X2,_,_,_,_,_,
                            _,X1,_,_,_,_,_,
                            _,X0,_,_,_,_,_],V):-positionVerticale(Player,[X0,X1,X2,X3,X4,X5],V).

positionVerticale(Player,2,[_,_,X5,_,_,_,_,
                            _,_,X4,_,_,_,_,
                            _,_,X3,_,_,_,_,
                            _,_,X2,_,_,_,_,
                            _,_,X1,_,_,_,_,
                            _,_,X0,_,_,_,_],V):-positionVerticale(Player,[X0,X1,X2,X3,X4,X5],V).

positionVerticale(Player,3,[_,_,_,X5,_,_,_,
                            _,_,_,X4,_,_,_,
                            _,_,_,X3,_,_,_,
                            _,_,_,X2,_,_,_,
                            _,_,_,X1,_,_,_,
                            _,_,_,X0,_,_,_],V):-positionVerticale(Player,[X0,X1,X2,X3,X4,X5],V).

positionVerticale(Player,4,[_,_,_,_,X5,_,_,
                            _,_,_,_,X4,_,_,
                            _,_,_,_,X3,_,_,
                            _,_,_,_,X2,_,_,
                            _,_,_,_,X1,_,_,
                            _,_,_,_,X0,_,_],V):-positionVerticale(Player,[X0,X1,X2,X3,X4,X5],V).

positionVerticale(Player,5,[_,_,_,_,_,X5,_,
                            _,_,_,_,_,X4,_,
                            _,_,_,_,_,X3,_,
                            _,_,_,_,_,X2,_,
                            _,_,_,_,_,X1,_,
                            _,_,_,_,_,X0,_],V):-positionVerticale(Player,[X0,X1,X2,X3,X4,X5],V).

positionVerticale(Player,6,[_,_,_,_,_,_,X5,
                            _,_,_,_,_,_,X4,
                            _,_,_,_,_,_,X3,
                            _,_,_,_,_,_,X2,
                            _,_,_,_,_,_,X1,
                            _,_,_,_,_,_,X0],V):-positionVerticale(Player,[X0,X1,X2,X3,X4,X5],V).

positionDiagonale(Player,M,[_,_,_,X3,_,_,_,
                           _,_,X2,_,_,_,_,
                           _,X1,_,_,_,_,_,
                           X0,_,_,_,_,_,_|_],V):-M<4,positionHorizontale(Player,M,[X0,X1,X2,X3,?,?,?],V).
positionDiagonale(Player,M,[_,_,_,_,X4,_,_,
                           _,_,_,X3,_,_,_,
                           _,_,X2,_,_,_,_,
                           _,X1,_,_,_,_,_,
                           X0,_,_,_,_,_,_|_],V):-M<5,positionHorizontale(Player,M,[X0,X1,X2,X3,X4,?,?],V).
positionDiagonale(Player,M,[_,_,_,_,_,X5,_,
                             _,_,_,_,X4,_,_,
                             _,_,_,X3,_,_,_,
                             _,_,X2,_,_,_,_,
                             _,X1,_,_,_,_,_,
                             X0,_,_,_,_,_,_|_],V):-M<6,positionHorizontale(Player,M,[X0,X1,X2,X3,X4,X5,?],V).
positionDiagonale(Player,M,[ _,_,_,_,_,_,X5,
                             _,_,_,_,_,X4,_,
                             _,_,_,_,X3,_,_,
                             _,_,_,X2,_,_,_,
                             _,_,X1,_,_,_,_,
                             _,X0,_,_,_,_,_],V):-M>0,positionHorizontale(Player,M,[?,X0,X1,X2,X3,X4,X5],V).
positionDiagonale(Player,M,[_,_,_,_,_,_,_,
                             _,_,_,_,_,_,X4,
                             _,_,_,_,_,X3,_,
                             _,_,_,_,X2,_,_,
                             _,_,_,X1,_,_,_,
                             _,_,X0,_,_,_,_],V):-M>1, positionHorizontale(Player,M,[?,?,X0,X1,X2,X3,X4],V).
positionDiagonale(Player,M,[_,_,_,_,_,_,_,
                             _,_,_,_,_,_,_,
                             _,_,_,_,_,_,X3,
                             _,_,_,_,_,X2,_,
                             _,_,_,_,X1,_,_,
                             _,_,_,X0,_,_,_],V):-M>2,positionHorizontale(Player,M,[?,?,?,X0,X1,X2,X3],V).
positionDiagonale(Player,M,[_,_,_,X0,_,_,_,
                             _,_,_,_,X1,_,_,
                             _,_,_,_,_,X2,_,
                             _,_,_,_,_,_,X3|_],V):-M>2,positionHorizontale(Player,M,[?,?,?,X0,X1,X2,X3],V).
positionDiagonale(Player,M,[_,_,X0,_,_,_,_,
                             _,_,_,X1,_,_,_,
                             _,_,_,_,X2,_,_,
                             _,_,_,_,_,X3,_,
                             _,_,_,_,_,_,X4|_],V):-M>1,positionHorizontale(Player,M,[?,?,X0,X1,X2,X3,X4],V).
positionDiagonale(Player,M,[_,X0,_,_,_,_,_,
                             _,_,X1,_,_,_,_,
                             _,_,_,X2,_,_,_,
                             _,_,_,_,X3,_,_,
                             _,_,_,_,_,X4,_,
                             _,_,_,_,_,_,X5],V):-M>0,positionHorizontale(Player,M,[?,X0,X1,X2,X3,X4,X5],V).
positionDiagonale(Player,M,[X0,_,_,_,_,_,_,
                             _,X1,_,_,_,_,_,
                             _,_,X2,_,_,_,_,
                             _,_,_,X3,_,_,_,
                             _,_,_,_,X4,_,_,
                             _,_,_,_,_,X5,_],V):-M<6,positionHorizontale(Player,M,[X0,X1,X2,X3,X4,X5,?],V).
positionDiagonale(Player,M,[_,_,_,_,_,_,_,
                             X0,_,_,_,_,_,_,
                             _,X1,_,_,_,_,_,
                             _,_,X2,_,_,_,_,
                             _,_,_,X3,_,_,_,
                             _,_,_,_,X4,_,_],V):-M<5,positionHorizontale(Player,M,[X0,X1,X2,X3,X4,?,?],V).
positionDiagonale(Player,M,[_,_,_,_,_,_,_,
                             _,_,_,_,_,_,_,
                             X0,_,_,_,_,_,_,
                             _,X1,_,_,_,_,_,
                             _,_,X2,_,_,_,_,
                             _,_,_,X3,_,_,_],V):- M<4,positionHorizontale(Player,M,[X0,X1,X2,X3,?,?,?],V).

%Test diagonale
%setof(Board,(length(Board,42),positionDiagonale(x,5,Board,10)),Boards),member(Board,Boards),assert(board(Board)),displayBoard,retract(board(Board)).


%minimax(0,Board, Flag,Move,Value):-
%    value(Board,V),
%    Value is V*Flag.%To delete soon
%minimax(Depth, Board, Flag, Move, Value):-
%    Depth>0,
%    setof(M,(move(Board,M)),Moves), %define move
%    DepthRecur is Depth-1,
%    otherFlag is -Flag,
%    evaluateAndChoose(Moves, Board, DepthRecur, otherFlag, (nil, -1000), (Move,Value)).



%evaluateAndChoose([],Board,Depth,Flag,Record,Record).
%evaluateAndChoose([Move|Moves], Board, Depth,Flag, Record, BestMoves):-
%    move(Move, Board, NewBoard),
%    minimax(Depth, NewBoard, Flag, MoveX,Value),%MoveX is useless, we don't need to know what to do afterwards
%    update(Move,Value,Record,Record1),
%    evaluateAndChoose(Moves,Board,Depth,Flag,Record1,BestMoves).


play(Player):-  write('New turn for:'), writeln(Player),
		board(Board), % instanciate the board from the knowledge base
	    displayBoard, % print it
            ia(Board, Move,Player), % ask the AI for a move, that is, an index for the Player
	    playMove(Board,Move,NewBoard,Player), % Play the move and get the result in a new Board
		    applyIt(Board, NewBoard), % Remove the old board from the KB and store the new one
	    changePlayer(Player,NextPlayer), % Change the player before next turn
            play(NextPlayer). % next turn!

ia(_, Move, x) :-writeln("Entrez l'indice de la colonne dans laquelle vous souhaitez jouer :"),read(M2), Move is M2-1.
ia(Board,Move,o):-repeat, Move is random(6), nth0(Move, Board, Elem), var(Elem), !.
positionInBoard(_,[],Move,Move).
positionInBoard(_,Board,Move,Move) :-nth0(Move,Board,Val),not(var(Val)),not(nth0(Move,Board,?)),!.
positionInBoard(Player,[_,_,_,_,_,_,_|Board],Move,Index):-positionInBoard(Player,Board,Move,I2),Index is I2+7.

%%%% Play a Move, the new Board will be the same, but one value will be instanciated with the Move
playMove(Board,Move,NewBoard,Player) :- Board=NewBoard,
    positionInBoard(Player,Board,Move,I2),
    Index is I2-7,
    Index>=0,
    nth0(Index,NewBoard,Player).

playMove(Moves,[X0,X1,X2,X3,X4,X5,X6|_]) :-setof(I,nth0(I,[X0,X1,X2,X3,X4,X5,X6],variable),Moves).

%%%% Remove old board/save new on in the knowledge base
applyIt(Board,NewBoard) :- retract(board(Board)), assert(board(NewBoard)).

%%%% Predicate to get the next player
changePlayer('x','o').
changePlayer('o','x').

%%%% Print the value of the board at index N:
% if its a variable, print ? and x or o otherwise.
printVal(N) :- board(B), nth0(N,B,Val), var(Val), write('?'), !.
printVal(N) :- board(B), nth0(N,B,Val), write(Val).

%%%% Display the board
displayBoard:-
    writeln('*-------------------------*'),
    printVal(0), write('  '), printVal(1), write('  '), printVal(2), write('  '), printVal(3), write('  '), printVal(4), write('  '), printVal(5), write('  '), printVal(6), writeln('  '),
    printVal(7), write('  '), printVal(8),write('  '), printVal(9), write('  '), printVal(10), write('  '), printVal(11),write('  '), printVal(12), write('  '), printVal(13), writeln(''),
    printVal(14), write('  '), printVal(15),write('  '), printVal(16),write('  '),  printVal(17),write('  '),  printVal(18),write('  '), printVal(19), write('  '), printVal(20), writeln(''),
    printVal(21), write('  '), printVal(22),write('  '), printVal(23),write('  '),  printVal(24),write('  '),  printVal(25),write('  '), printVal(26), write('  '), printVal(27), writeln(''),
    printVal(28), write('  '), printVal(29),write('  '), printVal(30),write('  '),  printVal(31), write('  '), printVal(32),write('  '), printVal(33), write('  '), printVal(34), writeln(''),
    printVal(35), write('  '), printVal(36),write('  '), printVal(37), write('  '), printVal(38),write('  '),  printVal(39),write('  '), printVal(40), write('  '), printVal(41), writeln(''),
    writeln('1 2 3 4 5 6 7'),
    writeln('*-------------------------*').

%%%%% Start the game!
init :- length(Board,42), assert(board(Board)), play('x').
