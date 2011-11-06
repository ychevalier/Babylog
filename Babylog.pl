%================================= Jeu =========================================

babylon :- player.

%===============================================================================

% ============================ Création des piles ==============================

/* Ces faits sont dynamiques car nous allons
 * les rendre faux et en créer de nouveaux.
 */

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

% Il existe quatre couleurs de piles.

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

%===============================================================================

% ======================= Petits prédicats utiles ==============================

/* Tete d'une liste.
 */
tete([T|_],T).

/* Liste de toutes les piles du jeu.
 */
get_piles(L) :- findall(Y,pile(Y),L).

/* Ajoute le premier paramètre completement
 * en fin de liste en second paramètre.
 * Résultat dans le troisième paramètre.
 * Différent de append qui ajoute le CONTENU.
 */
addm(L,[],[L]) :- !.
addm(L,[X|XS],R) :- addm(L,XS,Y), append([X],Y,R).

/* Ne supprime qu'un niveau de liste,
 * contrairement à flatten, qui supprime tout.
 */
flatten_nous([],[]).
flatten_nous([X|XS],L) :- flatten_nous(XS,Y), append(X,Y,L).

/* Minimum entre une liste et un élément.
 */
min([],E,E).
min([X|XS],E,M) :- X < E,!,min(XS,X,M).
min([_|XS],E,M) :- min(XS,E,M).

/* Maximum entre une liste et un élément.
 */
max([],E,E).
max([X|XS],E,M) :- X > E,!,max(XS,X,M).
max([_|XS],E,M) :- max(XS,E,M).

/* Tous les faits de la liste deviennent faux.
 */
retract_all([]).
retract_all([X|XS]) :- retract(pile(X)),retract_all(XS).

/* Tous les faits de la liste deviennent vrais.
 */
update_piles([]).
update_piles([X|XS]) :- assert(pile(X)),update_piles(XS).

/* Fonction pour imprimer la liste des piles.
 */
print_f([],_).
print_f([X|XS],IND) :- nl,write(IND),
					   write(' : '),
					   write(X),
					   Z is IND + 1,
					   print_f(XS,Z).

% ==============================================================================

/* TODO: Utiliser emplier dans empiler_list */

/* Le seul mouvement possible dans le jeu est l'empilement.
 * On peux empiler deux piles dont les sommets (la tête) sont
 * de la même couleurs, où qui possèdent le même nombre d'éléments.
 * 
 * empiler() Prend en paramètres les deux piles à empiler,
 * teste l'empilement, et donne au troisième paramètre R 0
 * si l'emplement est possible, -1 sinon.
 */
empiler(pile([X|XS]),pile(A),R) :- 	length(A,B),
									B == 1,
									length([X|XS],C),
									C > 1,
									last(XS,E),
									couleur(E,C1),
									tete(A,AS),
									couleur(AS,C2), 
									C1==C2,!,
									append([X|XS],[AS],L3),
									retract(pile(A)),
									retract(pile([X|XS])), 
									assert(pile(L3)),R is 0.						
empiler(pile(A),pile([X|XS]),R) :- 	length(A,B),
									B == 1,
									length([X|XS],C),
									C > 1,
									couleur(X,C1),
									tete(A,AS),
									couleur(AS,C2), 
									C1==C2,!,
									append([AS],[X|XS],L3),
									retract(pile(A)),
									retract(pile([X|XS])), 
									assert(pile(L3)),
									R is 0.
empiler(pile(L1), pile(L2),R) :-  	length(L1,X1),
									length(L2,X2), 
									X1==X2,!,
									append(L1, L2, L3),
									retract(pile(L2)),
									retract(pile(L1)), 
									assert(pile(L3)),
									R is 0.
empiler(pile(_), pile(_),R) :-    	nl,
									write('Impossible d\'empiler, essaie encore'),
									nl,
									nl, 
									R is -1.


/* Ce prédicat prend deux listes en paramètre,
 * et si leurs sommets sont de la même couleur,
 * ou si elles possèdent le même nombre d'élément,
 * alors elles sont concaténées.
 */
empiler_list([X|XS],A,L3) :- 		length(A,B),
									B == 1,
									length([X|XS],C),
									C > 1,!,
									last(XS,E),
									couleur(E,C1),
									tete(A,AS),
									couleur(AS,C2), 
									C1==C2,
									append([X|XS],[AS],Z),
									flatten(Z,L3).
empiler_list(A,[X|XS],L3) :- 		length(A,B),
									B == 1,
									length([X|XS],C),
									C > 1,
									couleur(X,C1),
									tete(A,AS),
									couleur(AS,C2), 
									C1==C2,!,
									append([AS],[X|XS],Z),
									flatten(Z,L3).
empiler_list([X|XS],[Y|YS],L3) :-  	length([X|XS],X1),
									length([Y|YS],X2), 
									X1==X2,!,
									append([X|XS],[Y|YS],L3).

%============================== Combinaisons ===================================

/* Combine deux éléments aux rangs I et J au sein d'une liste L 
 * dans une liste R.
 */
single_combination(I,J,L,R) :- 	I \== J, 
								nth0(I,L,E1),
								nth0(J,L,E2),
								empiler_list(E2,E1,R).					
														
/* Prédicat auxiliaire pour faire la combinaison des éléments en utilisant
 * les indices. On va parcourir la liste en faisant 
 * la combinaison des éléments suivant l' indice donné,
 * avec la reste des éléments.
 */
compl2(B,A,L,[]):- 	length(L,X),
					((A>=X);(B>=X)),!.
compl2(I,J,L,R) :- 	I\==J,
					single_combination(J,I,L,Y),!,
					A is J + 1,
					compl2(I,A,L,P),
					addm(Y,P,R).
compl2(I,J,L,R) :- 	A is J + 1,
					compl2(I,A,L,R).

/* Le problème de compl2, est que les éléments qui ne sont pas
 * compatibles avec l'élément que l'on teste n'aparaissent pas dans la liste.
 * les format_final simple et complexe s'occupent de ça.
 */
format_final_simple([],LR,[LR])  :- !.
format_final_simple([X|XS],LR,R) :- is_list(X),
									tete(X,T),
									not(member(T,LR)),!,
									format_final_simple(XS,LR,P),
									addm(X,P,R).
format_final_simple([X|XS],LR,R) :- is_list(X),
									format_final_simple(XS,LR,R).
format_final_simple([X|XS],LR,R) :- not(is_list(X)),
									not(member(X,LR)),!,
									format_final_simple(XS,LR,P),
									addm(X,P,R).
format_final_simple([X|XS],LR,R) :- not(is_list(X)),
									format_final_simple(XS,LR,R).

format_final_complexe(_,[],[])     :- !.
format_final_complexe(LO,[Y|YS],L) :- format_final_complexe(LO,YS,P),
									  format_final_simple(LO,Y,Z),
									  append([Z],P,L).

/* Pour un élément de la liste L à l'indice I donné,
 * Ce prédicat va donné toutes les combinaisons
 * possibles avec les autres éléments de L.
 */
complex_combination(I,L,R) :- 	compl2(I,0,L,P),
								format_final_complexe(L,P,R).

/* Avant dernier prédicat de la longue liste,
 * ce prédicat prend la liste des piles,
 * un index avec lequel appelé complex_combination,
 * récupère le résultat, le conserve et incrémente l'index.
 */
total2(I,L,[]) :- length(L,X),
				  I>=X,!.
total2(I,L,R)  :- complex_combination(I,L,Y),
				  A is I + 1,
				  total2(A,L,P),
				  addm(Y,P,R). 

/* Prédicat final de manipulation des piles!
 * Pour une pile donné, ce prédicat va nous donner tous les coups possibles !!
 */
total_combination(L,R)  :-  length(L,D),D > 1,
							total2(0,L,K),
							flatten_nous(K,R).
total_combination(L,[]) :-  length(L,D),
							((D < 1);(D == 1)).

%===============================================================================

/* A partir d'ici, si on fait appel à total_combination en lui donnant  comme
 * paramètre d'entrée un etat de jeux (une liste avec les piles
 * actuelles), il va rendre toutes les combinaisons possibles à partir
 * de la liste originale.
 */ 
 
/* Prédicat un peu pourri qui donne le maximum maximum d'une liste donné
 * si on lui donne 1 en param et le minimum si on lui donne -1.
 */
calculer_valeur(L,1,VN)  :-  max(L,-2,VN).
calculer_valeur(L,-1,VN) :-  min(L,2,VN).
 
/* Start et explore simulent 
 * une recherche en largeur (BFS) 
 * de tous les mouvements possibles.
 *
 * max = true <-> min = false
 * F   = fonction(min = -1 || max = 1)
 * VN  = valeur_node( +1 || -1)
 * L   = liste avec etat actuel du jeu.
 * VVN = tableau avec les valeurs du nodes au meme niveau.
 * calculer_valeur va faire le calcul du noeud parent en dépendance du
 * valeur de tous ses fils.(aussi en dependance de la fonction(min ou max)
 * min va calculer l'element minimum d'une liste---max le maximum.
 *
 * Start teste toutes les combinaisons
 * de premier niveaux, si il n'y a pas de mouvements possibles,
 * on arrete car on ne peut plus jouer (valeur VN = -1)
 */
start(L,F,_,VN,_)      :- total_combination(L,R),
						  R==[],!,
						  VN is (-1)*F.
start(L,F,LEVEL,VN,RF) :- total_combination(L,R),
						  explore(R,F,[],LEVEL,VN,RF).
						  
explore([],F,VVN,_,VN,_) 	  	  :- calculer_valeur(VVN,F,VN).
explore([X|_],F,_,LEVEL,1,_)  	  :- Z is (-1)*F,D is LEVEL + 1,
									 start(X,Z,D,K,_),
									 LEVEL \==0,
									 F == 1,K == 1,!.
explore([X|_],F,_,LEVEL,-1,_) 	  :- Z is (-1)*F,
									 D is LEVEL + 1 ,
									 start(X,Z,D,K,_),
									 LEVEL \== 0,
									 F == -1,K == -1,!.
explore([X|_],F,_,LEVEL,_,X)  	  :- Z is (-1)*F,
									 D is LEVEL + 1 ,
									 start(X,Z,D,K,_),
									 LEVEL == 0,K==1,!.
explore([X|XS],F,VVN,LEVEL,VN,RF) :- Z is (-1)*F,
									 D is LEVEL + 1 ,
									 start(X,Z,D,K,_),
									 append([K],VVN,R),
									 explore(XS,F,R,LEVEL,VN,RF).

/* Prédicat general qui fait le calcul de
 * la meilleure combination pour la machine.
 * S est l'indicateur de succès, 0 et la l'IA a joué, -1 
 * et il n'y avait pas de mouvement possible, et donc
 * le joueur a gagné.
 */
artificial_intelligence(L,R,S) :- start(L,1,0,K,P),
                                  K == -1,not(is_list(P)),
                                  total_combination(L,[R|_]),!, 
                                  S is 0.
artificial_intelligence(L,R,S) :- start(L,1,0,K,R),
								  K == -1,!,
								  S is -1.
artificial_intelligence(L,R,S) :- start(L,1,0,K,P),
								  K \== -1,
								  not(is_list(P)),!,
								  total_combination(L,[R|_]),
								  S is 0.
artificial_intelligence(L,R,S) :- start(L,1,0,_,R),
								  S is 0.

/* Applique le mouvement demandé par le joueur.
 */
player_internal_operations(L1,L2) :- empiler(pile(L1),pile(L2),R), 
									 R == 0,!,nl,nl,
									 write('Tour de l\'IA'),
									 machine.
player_internal_operations(_,_)   :- player.

/* Interface graphique pour le joueur.
 */
player :- get_piles(L),
		  total_combination(L,R),
		  R == [],!,
		  nl,print_f(L,0),
		  nl,write('Plus de mouvements possibles, PERDU!').
player :- nl,write('Etat du Jeu (INDEX : PILE):'),
		  get_piles(L),
		  print_f(L,0),nl,
		  write('Choisissez deux piles à combiner (N° de pile + "." + <entrée>):'),
		  nl,read(I),
		  read(J),
		  nth0(I,L,L1),
		  nth0(J,L,L2),!,
		  player_internal_operations(L1,L2).
player :- nl,write('Mouvement interdit, essayes encore.'),
		  player.

/* Prédicat qui controle l'IA.
 * Le nombre de mouvement est exponentiel,
 * et donc les premiers mouvements sont trop lent pour 
 * être joués tels quel. C'est pourquoi si il y a plus de 8 piles,
 * l'algo du FirstFit est utilisé.
 */
machine :- get_piles(L),
		   length(L,Z),Z>8,!,
		   total_combination(L,G),
		   length(G,P),
		   I is P - Z,
		   nth0(I,G,H),
		   retract_all(L),
		   update_piles(H),
		   player.
machine :- get_piles(L),
		   artificial_intelligence(L,R,CE),
		   CE == 0,!,
		   retract_all(L),
		   update_piles(R),
		   player.
machine :- nl,nl,write('Gagné!').


