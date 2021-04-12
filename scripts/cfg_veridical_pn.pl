% A CFG to generate simple sentences for Propositional Logic
% with semantic composition

:- use_module(countDepth,[countDepth/2,
                          countOccur/2,
                          subformula/2,
                          atomformula/2,
                          removeDuplicates/2]).
:- use_module(betaConversion,[betaConvert/2]).
:- use_module(fol2tptp,[fol2tptp/2]).
:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                printRepresentations/1]).

/* ==============================
   Main Rules
============================== */

%%% Simple Sentence %%%

s([parse:s(NP,VP),sem:SR,depth:0,sel:K]) -->
    !,
    np([parse:NP,sem:SR1,sel:K]),
    vp([parse:VP,sem:SR2,infl:past,neg:no,sel:K]),
    {combine(s:SR,[np:SR1,vp:SR2])}.

%%% Complex Sentence %%%

% Sentence negation
s([parse:s(Unary,S),sem:SR,depth:D1,sel:K]) -->
    {D2 is D1 - 1},
    unary([parse:Unary,sem:SR1,sel:K]),
    s([parse:S,sem:SR2,depth:D2,sel:K]),
    {combine(s:SR,[unary:SR1,s:SR2])}.

% % Sentence conjunction
% s([parse:s(S1,Binary,S2),sem:SR,depth:D1,sel:K]) -->
%     {D2 is D1 - 1},
%     s([parse:S1,sem:SR1,depth:D2,sel:K]),
%     binary([parse:Binary,sem:SR2,sel:K]),
%     s([parse:S2,sem:SR3,depth:D2,sel:K]),
%     {combine(s:SR,[s1:SR1,binary:SR2,s2:SR3])}.

% Noun Phrase
np([parse:np(PN),sem:SR,sel:K]) -->
    pn([parse:PN,sem:SR,sel:K]).

% Complex NP
np([parse:np(PN1,CONN,PN2),sem:SR,sel:K]) -->
    pn([parse:PN1,sem:SR1,sel:K]),
    conn([parse:CONN,sem:SR2,sel:K]),
    pn([parse:PN2,sem:SR3,sel:K]),
    {combine(np:SR,[pn1:SR1,conn:SR2,pn2:SR3])}.

np([parse:np(PN1,PN2,CONN,PN3),sem:SR,sel:K]) -->
    pn([parse:PN1,sem:SR1,sel:K]),
    punct,
    pn([parse:PN2,sem:SR2,sel:K]),
    conn([parse:CONN,sem:SR3,sel:K]),
    pn([parse:PN3,sem:SR4,sel:K]),
    {combine(np:SR,[pn1:SR1,pn2:SR2,conn:SR3,pn3:SR4])}.

% Verb Phrase
vp([parse:vp(IV),sem:SR,infl:I,neg:_,sel:K]) -->
    iv([parse:IV,sem:SR,infl:I,sel:K]).

vp([parse:vp(TV,NP),sem:SR,infl:I,neg:_,sel:K]) -->
    tv([parse:TV,sem:SR1,infl:I,sel:K]),
    np([parse:NP,sem:SR2,sel:K]),
    {combine(vp:SR,[tv:SR1,np:SR2])}.

vp([parse:vp(AUX,VP),sem:SR,infl:_,neg:no,sel:K]) -->
    aux([parse:AUX,sem:SR1,sel:K]),
    vp([parse:VP,sem:SR2,infl:base,neg:yes,sel:K]),
    {combine(vp:SR,[aux:SR1,vp:SR2])}.

% /* ==============================
%    Lexicon
% ============================== */

% Proper name
pn([parse:pn(Surf),sem:SR,sel:K]) -->
    {lex(pn,[surf:Surf])},
    Surf,
    {semlex(pn,[symbol:Surf,sem:SR])},
    {selector(K)}.

% Unary sentence operator
unary([parse:unary(Surf),sem:SR,sel:K]) -->
    {lex(unary,[surf:Surf])},
    Surf,
    {semlex(unary,[symbol:Surf,sem:SR])},
    {selector(K)}.

% Binary sentence operator
binary([parse:binary(Surf),sem:SR,sel:K]) -->
    {lex(binary,[surf:Surf])},
    Surf,
    {semlex(conn,[symbol:Surf,sem:SR])},
    {selector(K)}.

% Intransitive Verb
iv([parse:iv(Surf),sem:SR,infl:I,sel:K]) -->
    {lex(iv,[surf:Surf,symbol:Symbol,infl:I])},
    Surf,
    {semlex(iv,[symbol:Symbol,sem:SR])},
    {selector(K)}.

% Transitive Verb
tv([parse:tv(Surf),sem:SR,infl:I,sel:K]) -->
    {lex(tv,[surf:Surf,symbol:Symbol,infl:I])},
    Surf,
    {semlex(tv,[symbol:Symbol,sem:SR])},
    {selector(K)}.

% Auxiliary expressions
aux([parse:aux(Surf),sem:SR,sel:_]) -->
    {lex(aux,[surf:Surf])},
    Surf,
    {semlex(aux,[symbol:Surf,sem:SR])},
    {selector(1)}.

% NP connectives
conn([parse:conn(Surf),sem:SR,sel:_]) -->
    {lex(conn,[surf:Surf])},
    Surf,
    {semlex(conn,[symbol:Surf,sem:SR])},
    {selector(1)}.

% Punctuation
punct -->
    {lex(punct,[surf:Surf])},
    Surf.

% % Punctuation
% punct([parse:punct(Surf),sem:SR]) -->
%     {lex(punct,[surf:Surf])},
%     {semlex(punct,[symbol:Surf,sem:SR])},
%     Surf.

% /* ==============================
%   Lexical Entries
% ============================== */

% Proper name
% lex(pn,[surf:[np]]).
lex(pn,[surf:[ann]]).
lex(pn,[surf:[bob]]).
lex(pn,[surf:[chris]]).
lex(pn,[surf:[daniel]]).
lex(pn,[surf:[elliot]]).
lex(pn,[surf:[fred]]).
lex(pn,[surf:[greg]]).
lex(pn,[surf:[henry]]).
lex(pn,[surf:[tom]]).
lex(pn,[surf:[john]]).
% lex(pn,[surf:[mary]]).
% lex(pn,[surf:[kathy]]).
% lex(pn,[surf:[robert]]).
% lex(pn,[surf:[smith]]).
% lex(pn,[surf:[nancy]]).

% Intransitive verbs
lex(iv,[surf:[walk],symbol:[walk],infl:base]).
lex(iv,[surf:[walked],symbol:[walk],infl:past]).

lex(iv,[surf:[run],symbol:[run],infl:base]).
lex(iv,[surf:[ran],symbol:[run],infl:past]).

lex(iv,[surf:[swim],symbol:[swim],infl:base]).
lex(iv,[surf:[swam],symbol:[swim],infl:past]).

lex(iv,[surf:[sleep],symbol:[sleep],infl:base]).
lex(iv,[surf:[slept],symbol:[sleep],infl:past]).

lex(iv,[surf:[leave],symbol:[leave],infl:base]).
lex(iv,[surf:[left],symbol:[leave],infl:past]).

lex(iv,[surf:[exercise],symbol:[exercise],infl:base]).
lex(iv,[surf:[exercised],symbol:[exercise],infl:past]).

lex(iv,[surf:[jump],symbol:[jump],infl:base]).
lex(iv,[surf:[jumped],symbol:[jump],infl:past]).

lex(iv,[surf:[smoke],symbol:[smoke],infl:base]).
lex(iv,[surf:[smoked],symbol:[smoke],infl:past]).

lex(iv,[surf:[wave],symbol:[wave],infl:base]).
lex(iv,[surf:[waved],symbol:[wave],infl:past]).

lex(iv,[surf:[drink],symbol:[drink],infl:base]).
lex(iv,[surf:[drank],symbol:[drink],infl:past]).

% Transitive verbs
lex(tv,[surf:[see],symbol:[see],infl:base]).
lex(tv,[surf:[saw],symbol:[see],infl:past]).

lex(tv,[surf:[visit],symbol:[visit],infl:base]).
lex(tv,[surf:[visited],symbol:[visit],infl:past]).

lex(tv,[surf:[meet],symbol:[meet],infl:base]).
lex(tv,[surf:[met],symbol:[meet],infl:past]).

lex(tv,[surf:[hate],symbol:[hate],infl:base]).
lex(tv,[surf:[hated],symbol:[hate],infl:past]).

lex(tv,[surf:[touch],symbol:[touch],infl:base]).
lex(tv,[surf:[touched],symbol:[touch],infl:past]).

lex(tv,[surf:[admire],symbol:[admire],infl:base]).
lex(tv,[surf:[admired],symbol:[admire],infl:past]).

lex(tv,[surf:[respect],symbol:[respect],infl:base]).
lex(tv,[surf:[respected],symbol:[respect],infl:past]).

lex(tv,[surf:[find],symbol:[find],infl:base]).
lex(tv,[surf:[found],symbol:[find],infl:past]).

lex(tv,[surf:[love],symbol:[love],infl:base]).
lex(tv,[surf:[loved],symbol:[love],infl:past]).

lex(tv,[surf:[praise],symbol:[praise],infl:base]).
lex(tv,[surf:[praised],symbol:[praise],infl:past]).

% Unary and binary sentential operators
lex(unary,[surf:[it,is,not,the,case,that]]).
lex(binary,[surf:[and]]).
lex(binary,[surf:[or]]).

% VP negation
lex(aux,[surf:[did,not]]).

% NP conjunction/disjunction
lex(conn,[surf:[and]]).
lex(conn,[surf:[or]]).

% Punctuation
lex(punct,[surf:[punct]]).


% /* ==============================
%   Semantic Composition
% ============================== */

combine(s:SR,[np:NP,vp:VP]) :-
    SR = app(NP,VP).

combine(s:SR,[unary:Unary,s:S]) :-
    SR = app(Unary,S).

combine(s:SR,[s1:S1,binary:Binary,s2:S2]) :-
    SR = app(app(Binary,S1),S2).

combine(vp:SR,[tv:TV,np:NP]) :-
    SR = lam(X,app(NP,lam(Y,app(app(TV,Y),X)))).

combine(np:SR,[pn1:PN1,conn:Conn,pn2:PN2]) :-
    SR = lam(F,app(app(Conn,app(PN1,F)),app(PN2,F))).

combine(np:SR,[pn1:PN1,pn2:PN2,conn:Conn,pn3:PN3]) :-
    SR = lam(F,app(app(Conn,app(app(Conn,app(PN1,F)),app(PN2,F))),app(PN3,F))).

combine(vp:SR,[aux:AUX,vp:VP]) :-
    SR = lam(X,app(AUX,app(VP,X))).


% /* ==============================
%   Semantic Lexicon
% ============================== */

semlex(pn,[symbol:[Surf],sem:SR]) :-
    SR = lam(F,app(F,Surf)).

semlex(iv,[symbol:[Sym],sem:SR]) :-
    SR = lam(X,F),
    compose(F,Sym,[X]).

semlex(tv,[symbol:[Sym],sem:SR]) :-
    SR = lam(Y,lam(X,F)),
    compose(F,Sym,[X,Y]).

semlex(aux,[symbol:[did,not],sem:SR]) :-
    SR = lam(P,not(P)).

semlex(unary,[symbol:[it,is,not,the,case,that],sem:SR]) :-
    SR = lam(P,not(P)).

semlex(conn,[symbol:[and],sem:SR]) :-
    SR = lam(P,lam(Q,and(P,Q))).

semlex(conn,[symbol:[or],sem:SR]) :-
    SR = lam(P,lam(Q,or(P,Q))).

% semlex(punct,[symbol:_,sem:SR]) :-
%     SR = lam(X,X).


/* ==============================
  Auxiliary predicates
============================== */

yield([]).
yield([X|List]) :-
    write(X), write(' '), yield(List).

ptb(s(X,Y)) :-
    write('(S '), ptb(X), ptb(Y), write(')').

ptb(s(X,Y,Z)) :-
    write('(S '), ptb(X), ptb(Y), ptb(Z), write(')').

ptb(not(X)) :-
    write('(NOT '), ptb(X), write(')').

ptb(and(X,Y)) :-
    write('(AND '), ptb(X), ptb(Y), write(')').

ptb(np(X)) :-
    write('(NP '), ptb(X), write(')').

ptb(np(X,Y)) :-
    write('(NP '), ptb(X), ptb(Y), write(')').

ptb(np(X,Y,Z)) :-
    write('(NP '), ptb(X), ptb(Y), ptb(Z), write(')').

ptb(np(X,Y,Z,W)) :-
    write('(NP '), ptb(X), ptb(Y), ptb(Z), ptb(W), write(')').

ptb(pn(X)) :-
    write('(PN '), ptb(X), write(')').

ptb(vp(X)) :-
    write('(NP '), ptb(X), write(')').

ptb(vp(X,Y)) :-
    write('(VP '), ptb(X), ptb(Y), write(')').

ptb(aux(X)) :-
    write('(AUX '), ptb(X), write(')').

ptb(conn(X)) :-
    write('(CONN '), ptb(X), write(')').

ptb(unary(X)) :-
    write('(UNARY '), ptb(X), write(')').

ptb(binary(X)) :-
    write('(BINARY '), ptb(X), write(')').

ptb(iv([X|List])) :-
    write('(IV '), write(X), ptb(List).

ptb(tv([X|List])) :-
    write('(TV '), write(X), ptb(List).

ptb(punct([X|List])) :-
    write('(PUNCT '), write(X), ptb(List).

ptb([X|List]) :-
    write(' '), write(X), ptb(List).
ptb([]) :-
    write(')').

leq(N,N).
leq(_,0) :- !, fail.
leq(N1,N2):-
    M is N2 - 1, leq(N1,M).

le(N,M) :- leq(N,M), N =\= M.

selector(N) :- random_between(1,N,1).

compose(Term,Symbol,ArgList):-
    Term =.. [Symbol|ArgList].

nicePrint(SR):-
   \+ \+ (numbervars(SR,0,_), print(SR)).


/* Verbalize literals */

verbalize(not(P)) :-
  P =.. [F,X],
  !,
  write(X),
  write(" did not "),
  write(F).

verbalize(not(P)) :-
  P =.. [F,X,Y],
  !,
  write(X),
  write(" did not "),
  write(F),
  write(" "),
  write(Y).

verbalize(P) :-
  P =.. [F,X],
  !,
  write(X),
  write(" "),
  changeTense(iv,F,G),
  write(G).

verbalize(P) :-
  P =.. [F,X,Y],
  !,
  write(X),
  write(" "),
  changeTense(tv,F,G),
  write(G),
  write(" "),
  write(Y).

changeTense(iv,Symbol,Past) :-
  lex(iv,[surf:[Past],symbol:[Symbol],infl:past]).

changeTense(tv,Symbol,Past) :-
  lex(tv,[surf:[Past],symbol:[Symbol],infl:past]).



/* ==============================
   Main Predicates
============================== */

% Generate a plain sentence with depth D
plain(D,K) :-
   s([parse:_,sem:_,depth:D,sel:K],Sentence,[]),
   yield(Sentence),nl,
   fail.

% Generate a parse tree with depth D
gen(D,K) :-
   s([parse:Tree,sem:_,depth:D,sel:K],_,[]),
   % write(Tree),
   ptb(Tree),nl,
   fail.

% parsing
parse(D,S) :-
   s([parse:Parse,sem:_,depth:D,sel:1],S,[]),
   write(Parse),nl.

% semantic parsing
semparse(S) :- semparse(1,S).
semparse(S) :- semparse(0,S).

% enumurateLiteral(L,Atoms) :-
enumerateLiteral(Ls,[X|List]) :-
   Sub = [X,not(X)],
   enumerateLiteral(L1,List),
   append(L1,[Sub],Ls).

enumerateLiteral([],[]).

% semantic parsing
semparse(D,S) :-
   s([parse:_,sem:SR,depth:D,sel:1],S,[]),
   betaConvert(SR,NF),
   nicePrint(NF),
   write('\t'),
   countDepth(NF,Depth), write(Depth),
   write('\t'),
   countOccur(NF,Occur), write(Occur),
   write('\t'),
   atomformula(NF,Atoms),
   enumerateLiteral(Literals,Atoms),
   random_member(L1,Literals),
   random_member(F1,L1),
   random_member(L2,Literals),
   (L1 \== L2 ->
    random_member(F2,L2),
    removeDuplicates([F1,F2],Subs) ;
    Subs = [F1]),
    % random_member(L3,Literals),
    % append(L1,L2,SubList),
    % removeDuplicates(SubList,Subs),
    write(Subs),
    write('\t'),
    length(Subs,L),
    write(L),
    nl.

% Convert formulas to Sentences
verbalize(NF,D) :-
   s([parse:_,sem:SR,depth:D,sel:1],S,[]),
   betaConvert(SR,NF),
   write(S).
