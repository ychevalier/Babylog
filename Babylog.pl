couleur(noir1,n).
couleur(noir2,n).
couleur(noir3,n).

couleur(blanc1,b).
couleur(blanc2,b).
couleur(blanc3,b).

couleur(rouge1,r).
couleur(rouge2,r).
couleur(rouge3,r).

couleur(vert1,v).
couleur(vert2,v).
couleur(vert3,v).

:- dynamic pile/1.
pile([noir1]).
pile([noir2]).
pile([noir3]).

pile([blanc1]).
pile([blanc2]).
pile([blanc3]).

pile([rouge1]).
pile([rouge2]).
pile([rouge3]).

pile([vert1]).
pile([vert2]).
pile([vert3]).

tete([T|_],T).

empiler(pile(L1), pile(L2)) :- 	tete(L1,T1),couleur(T1,C1),
								tete(L2,T2),couleur(T2,C2), C1==C2,!,
								append(L1, L2, L3),
								retract(pile(L2)),retract(pile(L1)), assert(pile(L3)).

empiler(pile(L1), pile(L2)) :-  length(L1,X1),length(L2,X2), X1==X2,!,
								append(L1, L2, L3),
								retract(pile(L2)),retract(pile(L1)), assert(pile(L3)).

empiler(pile(_), pile(_)) :-    write(nope).

deux_prem(A,B,[A,B|R]).

is_fini() :- findall(Y,pile(Y),L),length(L,LEN),LEN >= 2,deux_prem(A,B,L), empiler(pile(A),pile(B)),!.
is_fini() :-

ia() :- findall(Y,pile(Y),L),length(L,LEN),LEN >= 2,deux_prem(A,B,L), empiler(pile(A),pile(B)),!.
ia() :-


