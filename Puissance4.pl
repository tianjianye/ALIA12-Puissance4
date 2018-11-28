:- dynamic board/1.
%value(PlayerFlag,Move,NewBoard,V):-
evaluer(_,[],1).
evaluer(_,[X|_],1):-var(X).
evaluer(Player,[X|_],1):-not(var(X)),X\==Player.
evaluer(Player,[_|L],V):-evaluer(Player,L,V2), V is V2*10.

construireBrancheG(M,M,_,[]).
construireBrancheG(M,I,[X|L],LG):-I2 is I+1, construireBrancheG(M,I2, L, LG2), append([X],LG2,LG).
construireBrancheD(_,_,[],[]).
construireBrancheD(M,I,[_|L],LD):-I=<M,I2 is I+1,construireBrancheD(M,I2, L,LD).
construireBrancheD(M,I,[X|L],LD):-I>M,I2 is I+1, construireBrancheD(M,I2,L,LD2), append([X],LD2,LD).

separerLigne(M,L,LG,LD):-construireBrancheG(M,0,L,LG),construireBrancheD(M,0,L,LD).

evaluerLigne(Player,M,L,V):-separerLigne(M,L,LG,LD),reverse(LG,GL),evaluer(Player,GL,VG),evaluer(Player,LD, VD), V is VG*VD.

positionHorizontale(Player,M,[X0,X1,X2,X3,X4,X5,X6|_],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val),not(var(Val)),evaluerLigne(Player,M,L,V).
positionHorizontale(Player, M, [X0,X1,X2,X3,X4,X5,X6|Board],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val),var(Val), positionHorizontale(Player,M,Board,V).
% Test positionHorizontale
%length(Board,7),positionHorizontale(x,6,Board,V).

construireVerticale(_,[],[]).
construireVerticale(M,[X0,X1,X2,X3,X4,X5,X6|Board],C):-nth0(M,[X0,X1,X2,X3,X4,X5,X6],Val), not(var(Val)),construireVerticale(M,Board,C2), append([Val],C2,C).
construireVerticale(M, [X0,X1,X2,X3,X4,X5,X6|Board],C):-nth0(M,[X0,X1,X2,X3,X4,X5,X6],Val),var(Val),construireVerticale(M,Board,C).
positionVerticale(Player,M,Board,V):-construireVerticale(M,Board,C),evaluer(Player,C,V).

construireDiagonaleG(_,[],[]).
construireDiagonaleG(H,[_|_],[]):-H<0.
construireDiagonaleG(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-not(nth0(H,[X0,X1,X2,X3,X4,X5,X6],_)),H>6,H2 is H-1,construireDiagonaleG(H2,Board,Diag).
construireDiagonaleG(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-nth0(H,[X0,X1,X2,X3,X4,X5,X6],Val), H2 is H-1,construireDiagonaleG(H2,Board,Diag2),
    append([Val],Diag2,Diag).
construireDiagonaleD(_,[],[]).
construireDiagonaleD(H,[_|_],[]):-H>6.
construireDiagonaleD(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-not(nth0(H,[X0,X1,X2,X3,X4,X5,X6],_)), H<0,H2 is H+1, construireDiagonaleD(H2,Board,Diag).
construireDiagonaleD(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-nth0(H,[X0,X1,X2,X3,X4,X5,X6],Val), H2 is H+1,construireDiagonaleD(H2,Board,Diag2),
    append([Val],Diag2,Diag).

hauteurJeton(_,[],0).
hauteurJeton(M,[X0,X1,X2,X3,X4,X5,X6|_],0):-nth0(M,[X0,X1,X2,X3,X4,X5,X6],Val),not(var(Val)).
hauteurJeton(M,[X0,X1,X2,X3,X4,X5,X6|Board],H):-nth0(M,[X0,X1,X2,X3,X4,X5,X6|Board],Val),var(Val),hauteurJeton(M,Board,H2),H is H2+1.

min([X],X).
min([X|L],M):-min(L,M2),M2<X, M is M2.
min([X|L],M):-min(L,M2),X<M2, M is X.

indexDiagG(M,H,IC,P):-IC is M+H, Reverse is 7-M,min([Reverse,H],P).
indexDiagD(M,H,IC,P):-IC is M-H, min([H,M],P).

positionDiagonale(Player,M,Board,V):-hauteurJeton(M,Board,H),
    indexDiagG(M,H,ICG,PG),construireDiagonaleG(ICG,Board,DiagG),evaluerLigne(Player,PG,DiagG,VG),
    indexDiagD(M,H,ICD,PD),construireDiagonaleD(ICD,Board,DiagD),evaluerLigne(Player,PD,DiagD,VD),
    V is VD+VG.

%length(Board,42),nth0(21,Board,x),nth0(15,Board,x),nth0(9,Board,x),nth0(3,Board,x),nth0(29,Board,x),nth0(37,Board,x),positionDiagonale(x,0,Board,V).
%Test diagonale
%setof(Board,(length(Board,42),positionDiagonale(x,5,Board,10)),Boards),member(Board,Boards),assert(board(Board)),displayBoard,retract(board(Board)).

%Valuations des distances

valeurDistance(2,20).
valeurDistance(3,10).
valeurDistance(4,5).
valeurDistance(X,0):-X<2.
valeurDistance(X,0):-X>4.
evaluerDistance(_,_,[],0).
evaluerDistance(Player,D,[X|L],V):-not(var(X)),X==Player,D2 is D+1,evaluerDistance(Player,D2,L,V2),valeurDistance(D,V3), V is V2+V3.
evaluerDistance(Player,D,[X|L],V):-(   var(X);not(var(X)),X\==Player),D2 is D+1, evaluerDistance(Player,D2,L,V).
%Test distance : length(A,7),nth0(0,A,x),nth0(1,A,x),nth0(2,A,x),nth0(3,A,x),nth0(4,A,x),nth0(5,A,x),nth0(6,A,x),trace,evaluerDistance(x,0,A,V).
evaluerDistanceLigne(Player,M,L,V):-separerLigne(M,L,LG,LD),reverse(LG,GL),evaluerDistance(Player,1,GL,VG),evaluerDistance(Player,1,LD,VD), V is VG +VD.

distanceHorizontale(Player,M,[X0,X1,X2,X3,X4,X5,X6|_],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val),not(var(Val)),evaluerDistanceLigne(Player,M,L,V).
distanceHorizontale(Player, M, [X0,X1,X2,X3,X4,X5,X6|Board],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val),var(Val), distanceHorizontale(Player,M,Board,V).

%Test de valuation : length(A,7),nth0(0,A,x),nth0(1,A,x),nth0(2,A,x),nth0(3,A,x),nth0(4,A,x),nth0(5,A,x),nth0(6,A,x),evaluerDistanceLigne(x,6,A,V).

distanceVerticale(Player,M,Board,V):-construireVerticale(M,Board,Col),evaluerDistance(Player,0,Col,V).

%Test distance Verticale : length(A,42),nth0(7,A,x),nth0(14,A,x),nth0(21,A,o),nth0(28,A,x),nth0(35,A,x),distanceVerticale(x,0,A,V).

evaluerDistanceDiagonaleG(Player,M,Board,V):-hauteurJeton(M,Board,H),indexDiagG(M,H,ICG,PG),construireDiagonaleG(ICG,Board,Diag),evaluerDistanceLigne(Player,PG,Diag,V).
evaluerDistanceDiagonaleD(Player,M,Board,V):-hauteurJeton(M,Board,H),indexDiagD(M,H,ICD,PD),construireDiagonaleD(ICD,Board,Diag),evaluerDistanceLigne(Player,PD,Diag,V).

distanceDiagonale(Player,M,Board,V):-.evaluerDistanceDiagonaleG(Player,M,Board,VG),evaluerDistanceDiagonaleD(Player,M,Board,VD),V is VG+VD.

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

playMove(Moves,[X0,X1,X2,X3,X4,X5,X6|_]) :-findall(I,nth0(I,[X0,X1,X2,X3,X4,X5,X6],variable),Moves).

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
