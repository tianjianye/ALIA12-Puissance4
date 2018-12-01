:- dynamic board/1.
%value(PlayerFlag,Move,NewBoard,V):-

%-----------------------------------------------ligne horizontale
construireGauche(M,M,_,[]).
construireGauche(M,M2,[X|L],LG):- M3 is M2+1,construireGauche(M,M3,L,LG2),append([X],LG2,LG).
construireDroite(_,_,[],[]).
construireDroite(M,M2,[X|L],LD):-M2>M,M3 is M2+1, construireDroite(M,M3,L,LD2), append([X],LD2,LD).
construireDroite(M,M2,[_|L],LD):-M2=<M,M3 is M2+1,construireDroite(M,M3,L,LD).
separerLigne(M, L, LG, LD):-construireGauche(M,0,L,LG),construireDroite(M,0,L,LD).

%evaluer(_,[],1).
%evaluer(_,[X|_],1):-var(X).
%evaluer(Player,[X|_],1):-not(var(X)),X\==Player.
%evaluer(Player,[X|L],V):-not(var(X)),X==Player, evaluer(Player,L,V2), V is V2*10.
valeurDeV(L,V):-length(L,P),P\==0,V is 10**P.
valeurDeV([],0).
evaluer(Player,L,V):-findall(I,(nth0(I,L,K),I<3,not(var(K)),K==Player),Indexes),valeurDeV(Indexes,V).

evaluerLigne(_,_,[],0).
evaluerLigne(Player,M,L,V):-separerLigne(M,L,LG,LD), reverse(LG,GL), evaluer(Player,GL,V2),evaluer(Player,LD,V3), V is V2*V3.

positionHorizontale(_,_,[],0).
positionHorizontale(Player, M, [X0,X1,X2,X3,X4,X5,X6|_],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val), Val==Player,not(var(Val)),evaluerLigne(Player,M,L,V).
positionHorizontale(Player, M, [X0,X1,X2,X3,X4,X5,X6|Board],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val),Val\==Player,positionHorizontale(Player, M, Board,V).
%length(Board,42),nth0(28,Board,x),nth0(35,Board,x),trace,positionHorizontale(x,Move,Board,V).
% Test positionHorizontale
%length(Board,35),nth0(28,Board,x),nth0(29,Board,x),nth0(30,Board,x),nth0(31,Board,x),trace,positionHorizontale(x,M,Board,V).
  
%-----------------------------------------------colonnes

construireVerticale(_,[],[]).
construireVerticale(M,[X0,X1,X2,X3,X4,X5,X6|Board],C):-nth0(M,[X0,X1,X2,X3,X4,X5,X6],Val),construireVerticale(M,Board,C2), append([Val],C2,C).
evaluerVerticale(Player,[X|_],0):-not(var(X)),X\==Player.
evaluerVerticale(Player,[X|C],V):-not(var(X)),X==Player,evaluer(Player,C,V).
evaluerVerticale(Player,[X|C],V):-var(X),evaluerVerticale(Player,C,V).
positionVerticale(_,M,Board,0):-hauteurJeton(M,Board,6).
positionVerticale(Player,M,Board,V):-construireVerticale(M,Board,Verticale),evaluerVerticale(Player,Verticale,V).

%Test vertical
%length(Board,35),nth0(0,Board,x),nth0(7,Board,x),nth0(14,Board,x),nth0(21,Board,o),nth0(28,Board,x),trace,positionVerticale(x,M,Board,V).


construireDiagonaleG(_,[],[]).
construireDiagonaleG(H,[_|_],[]):-H<0.
construireDiagonaleG(H,[_|_],[]):-H>8.
construireDiagonaleG(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-not(nth0(H,[X0,X1,X2,X3,X4,X5,X6],_)),H>6,H<9,H2 is H-1,construireDiagonaleG(H2,Board,Diag2),
    append([X],Diag2,Diag).
construireDiagonaleG(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-nth0(H,[X0,X1,X2,X3,X4,X5,X6],Val), H2 is H-1,construireDiagonaleG(H2,Board,Diag2),
    append([Val],Diag2,Diag).
construireDiagonaleD(_,[],[]).
construireDiagonaleD(H,[_|_],[]):-H>6.
construireDiagonaleD(H,[_|_],[]):- -2>H.
construireDiagonaleD(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-not(nth0(H,[X0,X1,X2,X3,X4,X5,X6],_)), H<0,-3<H,H2 is H+1, construireDiagonaleD(H2,Board,Diag2),
    append([X],Diag2,Diag).
construireDiagonaleD(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-nth0(H,[X0,X1,X2,X3,X4,X5,X6],Val), H2 is H+1,construireDiagonaleD(H2,Board,Diag2),
    append([Val],Diag2,Diag).

hauteurJeton(_,[],0).
hauteurJeton(M,[X0,X1,X2,X3,X4,X5,X6|_],0):-nth0(M,[X0,X1,X2,X3,X4,X5,X6],Val),not(var(Val)).
hauteurJeton(M,[X0,X1,X2,X3,X4,X5,X6|Board],H):-nth0(M,[X0,X1,X2,X3,X4,X5,X6|Board],Val),var(Val),hauteurJeton(M,Board,H2),H is H2+1.

indexDiagG(M,H,IC):-IC is M+H.
indexDiagD(M,H,IC):-IC is M-H.

positionDiagonale(Player,M,Board,V):-hauteurJeton(M,Board,H),
    indexDiagG(M,H,ICG),construireDiagonaleG(ICG,Board,DiagG),evaluerLigne(Player,H,DiagG,VG),
    indexDiagD(M,H,ICD),construireDiagonaleD(ICD,Board,DiagD),evaluerLigne(Player,H,DiagD,VD),
    V is VD+VG.

%Test diagonale
%length(Board,42),nth0(21,Board,x),nth0(15,Board,x),nth0(9,Board,x),nth0(3,Board,x),nth0(29,Board,x),nth0(37,Board,x),positionDiagonale(x,0,Board,V). %V=1000
%length(Board,42),nth0(22,Board,x),nth0(14,Board,x),nth0(30,Board,x),nth0(38,Board,x),nth0(16,Board,x),nth0(10,Board,x),nth0(4,Board,x),positionDiagonale(x,1,Board,V). %V=2000
%length(Board,42),nth0(23,Board,x),nth0(17,Board,x),nth0(15,Board,x),nth0(31,Board,x),positionDiagonale(x,2,Board,V).%V=110
%length(Board,42),nth0(24,Board,x),nth0(30,Board,x),nth0(32,Board,x),trace,positionDiagonale(x,3,Board,V).%V=20
%length(Board,42),nth0(18,Board,x),nth0(24,Board,x),nth0(12,Board,x),nth0(6,Board,x),nth0(10,Board,x),positionDiagonale(x,4,Board,V). %V=1010
%length(Board,42),nth0(19,Board,x),nth0(11,Board,x),nth0(3,Board,x),trace,positionDiagonale(x,5,Board,V).%V=100
%length(Board,42),nth0(27,Board,x),nth0(19,Board,x),nth0(11,Board,x),trace,positionDiagonale(x,6,Board,V).%V=100

%%%Valuations des distances

valeurDistance(2,20).
valeurDistance(3,10).
valeurDistance(4,5).
valeurDistance(X,0):-X<2.
valeurDistance(X,0):-X>4.
evaluerDistance(_,_,[],0).
evaluerDistance(Player,D,[X|L],V):-not(var(X)),X==Player,D2 is D+1,evaluerDistance(Player,D2,L,V2),valeurDistance(D,V3), V is V2+V3.
evaluerDistance(Player,D,[X|L],V):-(   var(X);not(var(X)),X\==Player),D2 is D+1, evaluerDistance(Player,D2,L,V).
%Test distance : length(A,7),nth0(0,A,x),nth0(1,A,x),nth0(2,A,x),nth0(3,A,x),nth0(4,A,x),nth0(5,A,x),nth0(6,A,x),trace,evaluerDistance(x,0,A,V).
evaluerDistanceLigne(_,_,[],0).
evaluerDistanceLigne(Player,M,L,V):-separerLigne(M,L,LG,LD),reverse(LG,GL),evaluerDistance(Player,1,GL,VG),evaluerDistance(Player,1,LD,VD), V is VG +VD.

distanceHorizontale(Player,M,[X0,X1,X2,X3,X4,X5,X6|_],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val),not(var(Val)),evaluerDistanceLigne(Player,M,L,V).
distanceHorizontale(Player, M, [X0,X1,X2,X3,X4,X5,X6|Board],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val),var(Val), distanceHorizontale(Player,M,Board,V).

%Test de valuation : length(A,7),nth0(0,A,x),nth0(1,A,x),nth0(2,A,x),nth0(3,A,x),nth0(4,A,x),nth0(5,A,x),nth0(6,A,x),evaluerDistanceLigne(x,6,A,V).

distanceVerticale(Player,M,Board,V):-construireVerticale(M,Board,Col),evaluerDistance(Player,0,Col,V).

%Test distance Verticale : length(A,42),nth0(7,A,x),nth0(14,A,x),nth0(21,A,o),nth0(28,A,x),nth0(35,A,x),distanceVerticale(x,0,A,V).

evaluerDistanceDiagonaleG(Player,M,Board,V):-hauteurJeton(M,Board,H),indexDiagG(M,H,ICG),construireDiagonaleG(ICG,Board,Diag),evaluerDistanceLigne(Player,H,Diag,V).
evaluerDistanceDiagonaleD(Player,M,Board,V):-hauteurJeton(M,Board,H),indexDiagD(M,H,ICD),construireDiagonaleD(ICD,Board,Diag),evaluerDistanceLigne(Player,H,Diag,V).

distanceDiagonale(Player,M,Board,V):-evaluerDistanceDiagonaleG(Player,M,Board,VG),evaluerDistanceDiagonaleD(Player,M,Board,VD),V is VG+VD.

getPlayer(1,'o').
getPlayer(-1,'x').

value(Board, Move, o, V):-Player='o',
    positionHorizontale(Player,Move,Board,V1),positionVerticale(Player,Move,Board,V2),positionDiagonale(Player,Move,Board,V3),
   distanceHorizontale(Player,Move,Board,V4),distanceVerticale(Player,Move,Board,V5),distanceDiagonale(Player,Move,Board,V6),
    V is V1+V2+V3+V4+V5+V6.
    
value(Board, Move, x, V):-Player='x',
   positionHorizontale(Player,Move,Board,V1),positionVerticale(Player,Move,Board,V2),positionDiagonale(Player,Move,Board,V3),
    V is (V1+V2+V3)/2.
sommeListe([X],X).
sommeListe([X|L],V):-sommeListe(L,V2), V is V2+X.

value(Board,V):-
    findall(V,(member(Move,[0,1,2,3,4,5,6]),value(Board,Move,o,V)),Values1),
    sommeListe(Values1,Vo),
    findall(V,(member(Move,[0,1,2,3,4,5,6]),value(Board,Move,x,V)),Values2),
    sommeListe(Values2,Vx),
    V is Vo+Vx.

minimax(0,Board, Flag,Move,Value):-
    value(Board,Value).
minimax(Depth, Board, Flag, Move, Value):-
    Depth>0,
    playMove(Moves, Board), %define move
    DepthRecur is Depth-1,
    OtherFlag is -1*Flag,
    evaluateAndChoose(Moves, Board, DepthRecur, OtherFlag, (nil, -10000), (Move,Value)).


evaluateAndChoose([],_,_,_,Record,Record).
evaluateAndChoose([Move|Moves], Board, Depth,Flag, Record, BestMoves):-
    getPlayer(Flag,Player),
    playMove(Board,Move, NewBoard,Player),
    minimax(Depth, NewBoard, Flag, _,Value1),%MoveX is useless, we don't need to know what to do afterwards
    Value is Value1*Flag,
    %write("Profondeur : "),write(Player), write(" valeur : "), write(Value), write("("),write(Move),writeln(")"),
    update(Move,Value,Record,Record1),
    evaluateAndChoose(Moves,Board,Depth,Flag,Record1,BestMoves).

update(_, Value, (Move1, Value1), (Move1,Value1)):-Value=<Value1.
update(Move, Value, (_, Value1), (Move, Value)):-Value > Value1.

win(Player) :- board(Board), positionHorizontale(Player,_,Board,1000),!.
win(Player) :- board(Board), positionVerticale(Player,_,Board,1000),!.
win(Player) :- board(Board), positionDiagonale(Player,_,Board,1000),!.
gameOver(o) :- win(o).
gameOver(x):-win(x).
gameOver("Draw"):-board(Board),playMove([],Board).


play(_):-gameOver(Winner),!,displayBoard,writeln("Fin de la partie, victoire : "),writeln(Winner).
%play(Player):-changePlayer(Player,NextPlayer),gameOver(NextPlayer),!,displayBoard,write('Victoire de '), write(NextPlayer), writeln(' !'),!.
play(Player):- write('New turn for:'), writeln(Player),
        board(Board), % instanciate the board from the knowledge base
        displayBoard, % print it
            chooseMove(Board, Move,Player), % ask the AI for a move, that is, an index for the Player
        playMove(Board,Move,NewBoard,Player), % Play the move and get the result in a new Board
            applyIt(Board, NewBoard), % Remove the old board from the KB and store the new one
        changePlayer(Player,NextPlayer), % Change the player before next turn
            play(NextPlayer). % next turn


%Pour tester, on joue x et o :
chooseMove(_, Move, x) :-writeln("Entrez l'indice de la colonne dans laquelle vous souhaitez jouer :"),read(M2), Move is M2-1.
%chooseMove(_, Move, Player) :-writeln("Entrez l'indice de la colonne dans laquelle vous souhaitez jouer :"),read(M2), Move is M2-1.
%% Ligne au dessus à commenter/dé-commenter
%
%chooseMove(Board,Move,o).%:-repeat, Move is random(6), nth0(Move, Board, Elem), var(Elem), !.
chooseMove(Board,Move,o):-playMove(Moves,Board),evaluateAndChoose(Moves, Board, 3, 1,(nil,-10000),(Move,Val)).

positionInBoard(_,[],Move,Move).
positionInBoard(_,Board,Move,Move) :-nth0(Move,Board,Val),not(var(Val)),not(nth0(Move,Board,?)),!.
positionInBoard(Player,[_,_,_,_,_,_,_|Board],Move,Index):-positionInBoard(Player,Board,Move,I2),Index is I2+7.

buildNewBoard([_|Board],0,[Player|Board],Player).
buildNewBoard([X|Board],I,NewBoard,Player):-I\==0, I2 is I-1, buildNewBoard(Board,I2,NewBoard2,Player),append([X],NewBoard2,NewBoard).

%%%% Play a Move, the new Board will be the same, but one value will be instanciated with the Move
playMove(Board,Move,NewBoard,Player) :-
    positionInBoard(Player,Board,Move,I2),
    Index is I2-7,
    Index>=0,
    buildNewBoard(Board,Index,NewBoard,Player).

playMove(Moves,[X0,X1,X2,X3,X4,X5,X6|_]) :-findall(I,(nth0(I,[X0,X1,X2,X3,X4,X5,X6],variable)),Moves).

%%%% Remove old board/save new on in the knowledge base
applyIt(Board,NewBoard) :- retract(board(Board)), assert(board(NewBoard)).

%%%% Predicate to get the next player
changePlayer('x','o').
changePlayer('o','x').

%%%% Print the value of the board at index N:
% if its a variable, print ? and x or o otherwise.
printVal(N) :- board(B), nth0(N,B,Val), var(Val), write('•'), !.
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
init :- length(Board,42), assert(board(Board)), play('x'),!.