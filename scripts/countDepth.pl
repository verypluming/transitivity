:- module(countDepth,[countDepth/2,
                      countOccur/2,
                      subformula/2,
                      atomformula/2,
                      countAtom/2,
                      countElement/3,
                      attachNot/2,
                      list2tptp/1,
                      printElement/1,
                      removeDuplicates/2]).

:- use_module(fol2tptp,[fol2tptp/2]).

/* Count the depth of a formula */

countDepth(and(P,Q),Depth):- !,
   countDepth(P,D1),
   countDepth(Q,D2),
   Depth is max(D1,D2) + 1.

countDepth(or(P,Q),Depth):- !,
   countDepth(P,D1),
   countDepth(Q,D2),
   Depth is max(D1,D2) + 1.

countDepth(not(P),Depth):- !,
   countDepth(P,D),
   Depth is D + 1.

countDepth(_,0).

/* Count the number of connectives */

countOccur(and(P,Q),Occur):- !,
   countOccur(P,Num1),
   countOccur(Q,Num2),
   Occur is Num1 + Num2 + 1.

countOccur(or(P,Q),Occur):- !,
   countOccur(P,Num1),
   countOccur(Q,Num2),
   Occur is Num1 + Num2 + 1.

countOccur(not(P),Occur):- !,
   countOccur(P,D),
   Occur is D + 1.

countOccur(_,0).

/* Count the number of np */

countElement(_,[],0).

countElement(E,[X|L],N):-
   E = X,
   !,
   countElement(E,L,N1),
   N is N1 + 1.

countElement(E,[_|L],N):-
   countElement(E,L,N).

/* Return a set of subformulas */

subformula(P,Set) :-
    !,
    subformulaList(P,List),
    delete(List,P,Res),
    list_to_set(Res,Set).

subformulaList(and(P,Q),Form) :-
    !,
    subformulaList(P,F1),
    subformulaList(Q,F2),
    append(F1,F2,F),
    append([and(P,Q)],F,Form).

subformulaList(or(P,Q),Form) :-
    !,
    subformulaList(P,F1),
    subformulaList(Q,F2),
    append(F1,F2,F),
    append([or(P,Q)],F,Form).

subformulaList(not(P),Form) :-
    !,
    subformulaList(P,F),
    append([not(P)],F,Form).

subformulaList(P,[P]).

/* Return a set of atomic formulas */

atomformula(P,Set) :-
    !,
    atomformulaList(P,Res),
    list_to_set(Res,Set).

atomformulaList(and(P,Q),Form) :-
    !,
    atomformulaList(P,F1),
    atomformulaList(Q,F2),
    append(F1,F2,Form).

atomformulaList(or(P,Q),Form) :-
    !,
    atomformulaList(P,F1),
    atomformulaList(Q,F2),
    append(F1,F2,Form).

atomformulaList(not(P),Form) :-
    !,
    atomformulaList(P,Form).

atomformulaList(P,[P]).

/* Count occurrences of atomic formulas */

countAtom(and(P,Q),N) :-
    !,
    countAtom(P,N1),
    countAtom(Q,N2),
    N is N1 + N2.

countAtom(or(P,Q),N) :-
    !,
    countAtom(P,N1),
    countAtom(Q,N2),
    N is N1 + N2.

countAtom(not(P),N) :-
    !,
    countAtom(P,N).

countAtom(_,1).

/* Attach negation to formuas */

attachNot([],[]).

attachNot([X|L],NegForm) :-
   !,
   attachNot(L,Res),
   append([not(X)],Res,NegForm).


/* Convert all elements of a list to tptp format */

list2tptp([]).

list2tptp([X|L]) :-
    fol2tptp(X,user),
    list2tptp(L).

/* Print all elements of a list */

printElement([]).

printElement([X|L]) :-
    write(X),nl,
    printElement(L).

% removeDuplicates(L1,L2) :- L2 is the result of removing duplicates in L1.

removeDuplicates([],[]).
removeDuplicates([X|L],Pruned):-
    member(Y,L),
    X==Y, !,
    removeDuplicates(L,Pruned).
removeDuplicates([X|L],[X|Pruned]):-
    removeDuplicates(L,Pruned).


% /* Verbalize literals */

% verbalize(not(P)) :-
%   P =.. [F,X],
%   !,
%   write(X),
%   write(" didn't "),
%   changeTense(F,G),
%   write(G),

% verbalize(not(P)) :-
%   P =.. [F,X,Y],
%   !,
%   write(X),
%   write(" didn't "),
%   changeTense(F,G),
%   write(G),
%   write(" "),
%   write(Y).

% verbalize(P) :-
%   P =.. [F,X],
%   !,
%   write(X),
%   write(" "),
%   write(F).

% verbalize(P) :-
%   P =.. [F,X,Y],
%   !,
%   write(X),
%   write(" "),
%   write(F),
%   write(" "),
%   write(Y).

% changeTense(Past,Present) :-
%   lex(tv,[surf:[Past],infl:past]),
%   lex(tv,[surf:[Present],infl:base]).
