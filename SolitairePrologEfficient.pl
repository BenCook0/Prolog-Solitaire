/* 1st row: out of bounds top */
/* 2nd and 3rd row, top corners */
/* 4th and 5th row, bottom corners */
/* 5th row, bottom out of bounds */
/* 6th and 7th row, left and right out of bounds */
/* this was phased out but still used for printing */
outbounds([-8,-7,-6,
           0,1,5,6,
           10,11,15,16,
           50,51,55,56,
           60,61,65,66,
           72,73,74,
           19,29,39,
           27,37,47]).


inbound(N):- 2 =< N, N =< 4.
inbound(N):- 12 =< N, N =< 14.
inbound(N):- 20 =< N, N =< 26.
inbound(N):- 30 =< N, N =< 36.
inbound(N):- 40 =< N, N =< 46.
inbound(N):- 52 =< N, N =< 54.
inbound(N):- 62 =< N, N =< 64.

/* used to print */
fullboard([0,1,2,3,4,5,6,
          10,11,12,13,14,15,16,
          20,21,22,23,24,25,26,
          30,31,32,33,34,35,36,
          40,41,42,43,44,45,46,
          50,51,52,53,54,55,56,
          60,61,62,63,64,65,66]).

/* used to check when to print an endline */
endboard([6,16,26,36,46,56,66]).

/*start state,goal position*/
crossbowBoard([31,32,   34,35,
               41,42,43,44,45,
                     53]).

crossbowGoal([3]).

longbowBoard([20,         26,
              30,31,33,35,36,
                41, 43, 45,
                 52,53,54,
                    63]).

longbowGoal([3]).

mostlydeadBoard([22,23,24,34,35,42,43,44]).

halfdeadBoard([22,23,24,34,35,42,43,44,
               20,30,40,41,52,62,54,64,45]).

notquitedeadBoard([2,3,4,
                  12,  14,
            20,21,22,23,24,25,26,
            30,   32,      35,36,
            40,41,42,43,44,45,46,
                  52,   54,
                  62,   64]).

fullB2([2,3,4,
        12,13,14,
  20,21,22,23,24,25,26,
  30,31,32,   34,35,36,
  40,41,42,43,44,45,46,
        52,53,54,
        62,63,64]).
/* used for mostly, half, almost dead and full board */
midGoal([33]).
/* ------------------- */
/* THIS IS WHAT THE TA SHOULD CALL TO RUN THE PROGRAM */
peg(crossbow):-
  crossbowSolver(_).

peg(longbow):-
  longbowSolver(_).

peg(mostlydead):-
  mostlydeadSolver(_).

peg(halfdead):-
  halfdeadSolver(_).

peg(notquitedead):-
  notquitedeadSolver(_).

peg(full):-
  fullSolver(_).

/* ------------------------- */

/* end of printing, write a newline */
printboard([],_):-
  write("\n").
/*F|FB is the "full board", CB is the current board, F is a "middle cell" */
printboard([F|FB],CB):-
  endboard(Y),
  not(member(F,Y)),
  printcell(F,CB),
  printboard(FB,CB).

printboard([F|FB],CB):-
  endboard(Y),
  member(F,Y),
  printcell(F,CB),
  write("\n"),
  printboard(FB,CB).

/* print an A in occupied spots */
printcell(A,CB):-
  member(A,CB),
  write("A").

/* print an X is out of bounds spots */
printcell(A,_):-
  outbounds(X),
  member(A,X),
  write(" ").

/* print . in unoccupied spots that are in bounds */
printcell(A,CB):-
  outbounds(X),
  not(member(A,CB)),
  not(member(A,X)),
  write(".").

/* ------------------------- */

/*if the rest of list match other than X then true */
myremove(X,[X|YS],YS).
myremove(X,[Y|YS],[Y|ZS]):-
    myremove(X, YS, ZS).

/* ---------------------- */

/*from, jumped end*/
jumpright(Start,Jumped,End):- 
  Jumped is Start+1,
  End is Start+2,
  inbound(Start),
  inbound(Jumped),
  inbound(End).

/*from, jumped end*/
jumpleft(Start,Jumped,End):- 
  Jumped is Start-1,
  End is Start-2,
  inbound(Start),
  inbound(Jumped),
  inbound(End).

/*from, jumped end*/
jumpup(Start,Jumped,End):- 
  Jumped is Start-10,
  End is Start-20,
  inbound(Start),
  inbound(Jumped),
  inbound(End).

/*from, jumped end*/
jumpdown(Start,Jumped,End):- 
  Jumped is Start+10,
  End is Start+20,
  inbound(Start),
  inbound(Jumped),
  inbound(End).

/* all jumps */
jump(Start,Jumped,End):-
  jumpright(Start,Jumped,End);
  jumpleft(Start,Jumped,End);
  jumpup(Start,Jumped,End);
  jumpdown(Start,Jumped,End).

/* solitaire move */
solitaire_move(SB,(Start,End),[End|SB2]):-
  myremove(Start,SB,SB1),
  jump(Start,Jumped,End),
  myremove(Jumped,SB1,SB2),
  not(member(End,SB2)).

solitaire_steps(G,SB,[Mv|Moves],GB,Hist):-
  solitaire_move(SB,Mv,SB1),
  independanceCheck(Mv,Hist),
  findall((P,W),(member(P,[simple,center,centerF]),wgt(P,SB1,W)),Wgts),
  checkWgt(G,Wgts),
  solitaire_steps(G,SB1,Moves,GB,[Mv|Hist]).

solitaire_steps(_,GB,[],GB,_).

/* -------------------- */
printfull([],_).
printfull([(Start,End)|MVs],SB):-
    fullboard(Full),
    midCalc((Start,End),Mid),
    myremove(Start,SB,SB1),
    myremove(Mid,SB1,SB2),
    append([End],SB2,SB3),
    printboard(Full,SB3),
    printfull(MVs,SB3).

/* -------------------- */

solitaire_stepsSetup(full,SB,X,GB,_):-
    fullboard(Full),
    fullB2(Z),
    fulltomostly(Q),
    write("Start Board: \n"),
    printboard(Full,Z),
    solitaire_steps(full,SB,X,GB,[]),
    write("steps:\n"),
    printfull(Q,Z),
    printboard(Full,SB),
    printfull(X,SB),
    write("Goal board:\n"),
    printboard(Full,GB),
    !.

solitaire_stepsSetup(G,SB,X,GB,_):-
    fullboard(Full),
    write("Start Board: \n"),
    printboard(Full,SB),
    solitaire_steps(G,SB,X,GB,[]),
    write("steps:\n"),
    printfull(X,SB),
    write("Goal board:\n"),
    printboard(Full,GB),
    !.

/* ------------------- */

/* solvers */
crossbowSolver(X):-
  crossbowBoard(SB),
  crossbowGoal(GB),
  solitaire_stepsSetup(crossbow,SB,X,GB,[]).

longbowSolver(X):-
  longbowBoard(SB),
  longbowGoal(GB),
  solitaire_stepsSetup(longbow,SB,X,GB,[]).

mostlydeadSolver(X):-
  mostlydeadBoard(SB),
  midGoal(GB),
  solitaire_stepsSetup(mostlydead,SB,X,GB,[]).

fullSolver(X):-
  notquitedeadBoard(SB),
  midGoal(GB),
  solitaire_stepsSetup(full,SB,X,GB,[]).  

halfdeadSolver(X):-
  halfdeadBoard(SB),
  midGoal(GB),
  solitaire_stepsSetup(halfdead,SB,X,GB,[]).

notquitedeadSolver(X):-
  notquitedeadBoard(SB),
  midGoal(GB),
  solitaire_stepsSetup(notquitedead,SB,X,GB,[]).



/* --------------------------- */
/* independance checking complete */
independanceCheck(_,[]).
independanceCheck(Move,[H|_]):- overlap(Move,H),!.
independanceCheck(Move,[H|Hist]):- lexorder(Move,H),
                                    independanceCheck(H,Hist).
                          
/* arbitrary lex order */
lexorder((S1,E1),(S2,E2)):-
  S1 < S2,!;
  (S1 == S2,
  E1 =< E2,!).

/* Check if a move overlaps with another move */
overlap((Start,End),(Start2,End2)):-
    (midCalc((Start,End),Mid),
    midCalc((Start2,End2),Mid2)),
    (Start == Start2;
    Start == Mid2;
    Start == End2;
    Mid == Start2;
    Mid == Mid2;
    Mid == End2;
    End == Start2;
    End == Mid2;
    End == End2).

midCalc((Start,End),Midc2):-
    Midc1 is Start + End,
    Midc2 is Midc1 / 2.

/* ------------------------ */
/* pagoda function logic */
wgt(_,[],0).
wgt(P,[Pa|Rest],Wgt):-
  (pagoda(P,Pa,Pwgt);
  Pwgt = 0),!,
  wgt(P,Rest,WgtRest),
  Wgt is WgtRest + Pwgt.

pagodaTest(X):-
  crossbowBoard(B),
  wgt(simple,B,X).

checkWgt(_,[]).
checkWgt(G,[(P,Wgt)|Rest]):-
  goalwgt(G,P,WgtGoal),
  Wgt >= WgtGoal,
  checkWgt(G,Rest).

/* ------------------------ */
/* pagoda function database */
/* NOTE that some of these pagoda functions were defined
but were not implemented as they did not have a noticable impact on run time,
they were kept in this database for the sake of completeness */

/* goal weights simple */
goalwgt(crossbow,simple,0).
goalwgt(longbow,simple,0).
goalwgt(mostlydead,simple,1).
goalwgt(halfdead,simple,1).
goalwgt(notquitedead,simple,1).
goalwgt(full,simple,1).

goalwgt(crossbow,center,0).
goalwgt(longbow,center,0).
goalwgt(mostlydead,center,2).
goalwgt(halfdead,center,2).
goalwgt(notquitedead,center,2).
goalwgt(full,center,2).

goalwgt(crossbow,third,0).
goalwgt(longbow,third,0).
goalwgt(mostlydead,third,0).
goalwgt(halfdead,third,0).
goalwgt(notquitedead,third,0).
goalwgt(full,third,0).

goalwgt(crossbow,top,21).
goalwgt(longbow,top,21).
goalwgt(mostlydead,top,5).
goalwgt(halfdead,top,5).
goalwgt(notquitedead,top,5).
goalwgt(full,top,5).

goalwgt(crossbow,centerF,0).
goalwgt(longbow,centerF,0).
goalwgt(mostlydead,centerF,2).
goalwgt(halfdead,centerF,2).
goalwgt(notquitedead,centerF,2).
goalwgt(full,centerF,2).

goalwgt(crossbow,thirdF,0).
goalwgt(longbow,thirdF,0).
goalwgt(mostlydead,thirdF,0).
goalwgt(halfdead,thirdF,0).
goalwgt(notquitedead,thirdF,0).
goalwgt(full,thirdF,0).
fulltomostly([(31,33),(34,32),(13,33),(43,23)]).
/* simple */
pagoda(simple,13,1).
pagoda(simple,31,1).
pagoda(simple,33,1).
pagoda(simple,35,1).
pagoda(simple,53,1).

/* center */
pagoda(center,13,1).

pagoda(center,20,-1).
pagoda(center,21,1).
pagoda(center,23,1).
pagoda(center,25,1).
pagoda(center,26,-1).

pagoda(center,31,2).
pagoda(center,33,2).
pagoda(center,35,2).

pagoda(center,40,-1).
pagoda(center,41,1).
pagoda(center,43,1).
pagoda(center,45,1).
pagoda(center,46,-1).

pagoda(center,53,1).

/* third Useless in every case???*/
pagoda(third,2,-1).
pagoda(third,4,-1).

pagoda(third,12,1).
pagoda(third,14,1).

pagoda(third,31,1).
pagoda(third,32,1).
pagoda(third,34,1).
pagoda(third,36,1).

pagoda(third,52,1).
pagoda(third,54,1).

pagoda(third,62,-1).
pagoda(third,64,-1).

/* top weighted */
pagoda(top,3,21).

pagoda(top,13,13).

pagoda(top,20,-8).
pagoda(top,21,8).
pagoda(top,23,8).
pagoda(top,25,8).
pagoda(top,26,-8).

pagoda(top,30,5).
pagoda(top,31,5).
pagoda(top,33,5).
pagoda(top,35,5).
pagoda(top,36,5).

pagoda(top,40,-3).
pagoda(top,41,3).
pagoda(top,43,3).
pagoda(top,45,3).
pagoda(top,46,-3).

pagoda(top,53,2).

pagoda(top,63,1).

/* center but flipped */
pagoda(centerF,2,-1).
pagoda(centerF,4,-1).

pagoda(centerF,12,1).
pagoda(centerF,13,2).
pagoda(centerF,14,1).

pagoda(centerF,31,1).
pagoda(centerF,32,1).
pagoda(centerF,33,2).
pagoda(centerF,34,1).
pagoda(centerF,35,1).

pagoda(centerF,52,1).
pagoda(centerF,53,2).
pagoda(centerF,54,1).

pagoda(centerF,62,-1).
pagoda(centerF,64,-1).

/* third but flipped */
pagoda(thirdF,13,1).

pagoda(thirdF,20,-1).
pagoda(thirdF,21,1).
pagoda(thirdF,23,1).
pagoda(thirdF,25,1).
pagoda(thirdF,26,-1).

pagoda(thirdF,40,-1).
pagoda(thirdF,41,1).
pagoda(thirdF,43,1).
pagoda(thirdF,45,1).
pagoda(thirdF,46,-1).

pagoda(thirdF,63,1).
