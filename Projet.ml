(*
	PROJET - PROGRAMMATION FONCTIONNELLE
		JACK HOGG - 18000088
*)

type expr_reg = Vide 
		| Epsilon 
		| S of char 
		| Ou of expr_reg * expr_reg 
		| Concat of expr_reg * expr_reg
		| Etoile of expr_reg ;;

(*	AFD

  - nombre d'états (commençant à 0) ;
  - état initial ;
  - liste états acceptants ;
  - fonction de transition (val etat : int, val ls : list) ;

	Initial : Utiliser des caractères (type char)
	Général : Expressions régulières

	États acceptants : Supprimer un à un les états "intermédiaires"
		==> Expressions régulières
*)

(* 	FONCTIONS UTILES

  - List.mem : Vérifier l'appartenance d'un élément à une liste
  - List.map : Appliquer une fonction à tous les éléments d'une liste
  - List.fold_left / fold_right : 
  - List.find : Retourne le premier élément qui satisfait un prédicat
  - List.find_all : Retourne tous les éléments qui valident le prédicat
  - List.exists : Vérifie si au moins un élément valide le prédicat p
  - List.for_all : Teste un prédicat sur une liste


*)


(* Exemple du sujet *)
let trans = [(0,[('a',0);('b',1)]);(1,[('b',1);('a',2)]);(2,[('a',2);('b',3)]);(3,[('a',0);('b',3)])] ;;

(* 1. Généraliser un AFD à partir d'un graphe *)
let rec char_en_expr_reg c = match c with
	[] -> []
	| (c, et)::a -> (S(c), et)::(char_en_expr_reg a)
;;

char_en_expr_reg [('a',0)] ;;

let rec generaliser_afd graphe = match graphe with
	[] -> []
	| (e, t)::r -> (e, char_en_expr_reg t)::(generaliser_afd r)
;;


(* Test de la fonction generaliser_afd *)
let trans_gene = generaliser_afd trans ;;


(* 2. Retourner une chaîne de caractères à partir d'un graphe *)
let rec expr_en_chaine er = match er with
	Vide -> ""
	| Epsilon -> ""
	| S(c) -> Char.escaped c
	| Ou(er_g, er_d) -> expr_en_chaine er_g ^ "|" ^ expr_en_chaine er_d
	| Concat(er_g, er_d) -> expr_en_chaine er_g ^ expr_en_chaine er_d
	| Etoile(er_g) -> expr_en_chaine er_g ^ "*" ;;

let expr_1 = Ou(S('c'), Concat(Concat(S('b'), Etoile(S('b'))) , S('a'))) ;;

let trans_test = expr_en_chaine expr_1 ;;


(* 3. Éliminer un état d'un graphe généralisé, ajouter les transitions *)

(* Fonction annexe : Récupère les transitions de l'état eliminé *)
let rec transitions_etat etat graphe = match graphe with
	[] -> []
	| (e, t)::r -> if e = etat then t else transitions_etat etat r ;;

let trans_elim = transitions_etat 1 trans_gene ;;

(* Fonction annexe : Transforme la fonction de transition de l'état supprimé vers lui-même *)
let modif_trans_meme_etat etat couple = match couple with
	(t, e) -> if e = etat then (Etoile(t), e) else (t,e) ;;

modif_trans_meme_etat 1 (S 'a', 1) ;;

(* Fonction annexe : Transformer les transitions de l'état supprimé *)
let rec trans_etat_supp etat transitions = match transitions with
	[] -> []
	| c::r -> (modif_trans_meme_etat etat c)::(trans_etat_supp etat r) ;;

let trans_modif_etat = trans_etat_supp 1 trans_elim ;;


(* Fonction annexe : AFD sans l'état éliminé *)
let rec afd_sans_etat_elim etat graphe = match graphe with
	[] -> []
	| (e, t)::r -> let g = (afd_sans_etat_elim etat r) in if e = etat then g else (e, t)::g ;;

let sans_1 = afd_sans_etat_elim 1 trans_gene ;;

(* Fonction annexe : Trouver la fonction de transition qui correspond à l'état éliminé, dans les prédecesseurs de l'état
	i.e. On a [(0,[('a',0);('b',1)]); <- L'ÉTAT 0 EST UN PREDECESSEUR
		(1,[('b',1);('a',2)]); <- ON SOUHAITE ELIMINER CET ETAT
		(2,[('a',2);('b',3)]);
		(3,[('a',0);('b',3)])]
*)
let rec trouver_predecesseur etat trans = match trans with
	[] -> false
	| (c, e)::r -> if e = etat then true else trouver_predecesseur etat r ;;

(* PROBLEM : 
	Transform 	(0,[(S 'a',0);(S 'b',1)])  	 into ->       (0, [(Concat (S 'b', Concat (Etoile (S 'b'), S 'a')), 2)
	
	So I transform the eliminated state into a list, so I get (1,[(S 'b',1);(S 'a',2)]) -> [(Etoile(S 'b'),1);(S 'a', 2)] with the function trans_etat_supp
	and I use that list to transform the state 0 but the problem is essentially if there is more than one transition in the state 1. I don't know how to treat
	this problem as generaly as I can.
	
	I use the function afd_sans_etat_elim in order to get the AFD without the eliminated state.
	
	Maybe I do it all the wrong way but I believe I have to transform the eliminated state first and then study the other states and find if there are any predecessors
	so that I can obtain a graph (AFD) withtout that state.

*)
let rec transformer_etat trans = match trans with
	[] -> []
	| 

(* Eliminer un état et renoyer le graphe correspondant *)
let rec eliminer_un_etat etat graphe = 
	let graphe_bis = afd_sans_etat_elim etat graphe in
		match graphe_bis with
			[] -> []
			| (e, t)::r -> if trouver_predecesseur etat t then (e, transformer_etat t)::(eliminer_un_etat etat r)
					else (e,t)::(eliminer_un_etat etat r)
;;

eliminer_un_etat 1 trans_gene ;;



(* 4. Réduire un graphe à un état initial et un état final et retourner l'expression régulière *)

let rec eliminer_tous etats_sauv nb graphe =

;;

let trans_reduit = eliminer_tous [0;3] 4 trans_gene ;


(* 5. A partir d'un AFD, donner une expression régulière *)
let rec expression_reguliere etat_init etat_fin graphe =

;;

let er03 = expression_reguliere 0 3 trans_reduit ;;


(* FINAL : Convertir une expression régulière en chaîne de caractères *)
let expr_reg_to_string er = 

;;

*)
