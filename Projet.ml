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

let rec generaliser_afd transitions = match transitions with
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
	| Ou(er_g, er_d) -> "(" ^ expr_en_chaine er_g ^ "|" ^ expr_en_chaine er_d ^ ")"
	| Concat(er_g, er_d) -> expr_en_chaine er_g ^ expr_en_chaine er_d
	| Etoile(er_g) -> expr_en_chaine er_g ^ "*" ;;

let expr_1 = Ou(S('c'), Concat(Concat(S('b'), Etoile(S('b'))) , S('a'))) ;;

let trans_test = expr_en_chaine expr_1 ;;






(* 3. Éliminer un état d'un graphe généralisé, ajouter les transitions *)

(* Fonction annexe : Récupère les transitions de l'état eliminé *)
let rec transitions_etat etat transitions =
	let p = List.find (fun (x, y) -> etat = x) transitions in snd p ;;


(* Fonction annexe : Transforme la fonction de transition de l'état supprimé vers lui-même *)
let modif_trans_meme_etat etat transition = match transition with
	(t, e) -> if e = etat then (Etoile(t), e) else (t,e) ;;


(* Fonction annexe : Transformer les transitions de l'état supprimé *)
let rec trans_etat_supp etat transitions = match transitions with
	[] -> []
	| c::r -> (modif_trans_meme_etat etat c)::(trans_etat_supp etat r) ;;


(* Fonction annexe : Trouver la fonction de transition qui correspond à l'état éliminé, dans les prédecesseurs de l'état *)
let rec trouver_predecesseur etat_elimine transitions = match transitions with
	[] -> false
	| (c, e)::r -> if e = etat_elimine then true else trouver_predecesseur etat_elimine r ;;


(* Fonction annexe : Extraire des couples les transitions transformées (transitions de l'état supprimé vers l'état lui-même) *)
let rec meme_etat_trans etat_elimine trans_extrait = match trans_extrait with
	[] -> []
	| (t, e)::r -> if e = etat_elimine then (t, e)::(meme_etat_trans etat_elimine r) else meme_etat_trans etat_elimine r
;;



(* Fonction annexe : Extraire des couples les transitions non-transformées (transitions de l'état supprimé vers un autre état) *)
let rec diff_etat_trans etat_elimine trans_extrait = match trans_extrait with
	[] -> []
	| (t, e)::r -> if e = etat_elimine then diff_etat_trans etat_elimine r else (t, e)::(diff_etat_trans etat_elimine r)
;;



(* Paramètres : Etat courant, Etat supprimé, Tête tu couple gardé courant, Regex de l'état éliminé, Translations du noeud courant *)
let rec transformer_etat_ etat etat_supp tt eli transitions = match transitions with
	[] -> []
	| (t, e)::r -> if e = etat_supp then (Concat(t, Concat(eli, tt)), etat)::r
			else (t, e)::(transformer_etat_ etat etat_supp tt eli r)
;;

(* Paramètres : Etat supprimé, Translations du noeud courant, Regex de l'état éliminé, couple gardé courant *)
let rec transformer_etat etat_supp transitions eli t_gard = match t_gard with
	[] -> []
	| (t, e)::r -> let p = (transformer_etat_ e etat_supp t eli transitions) in (* Je crée une liste de transitions *)
			p@(transformer_etat etat_supp transitions eli r)
;;

(* Eliminer un état et renvoyer le graphe correspondant *)
let rec eliminer_un_etat etat graphe = 
		match graphe with
			[] -> []
			| (e, t)::r -> if e = etat then eliminer_un_etat etat r else 
						if trouver_predecesseur etat t then 
							let t_eli = fst (List.hd (trans_etat_supp etat (meme_etat_trans etat (transitions_etat etat graphe)))) in 
								let t_gard = diff_etat_trans etat (transitions_etat etat graphe) in
								 	(e, transformer_etat etat t t_eli t_gard)::(eliminer_un_etat etat r)
						else (e,t)::(eliminer_un_etat etat r)
;;

eliminer_un_etat 3 trans_gene ;;




(* 4. Réduire un AFD initial en des états initial et final et retourner l'expression régulière correspondante *)

(* Fonction : Eliminer les états "intermédiaires" *)
let rec eliminer_tous intervalle nb_etats transitions =
	if (nb_etats > 0) then 
		if (List.mem (nb_etats - 1) intervalle) then eliminer_tous intervalle (nb_etats - 1) transitions
		else let p = eliminer_un_etat (nb_etats - 1) transitions in
			eliminer_tous intervalle (nb_etats - 1) p
	else transitions
			
;;

let trans_reduit = eliminer_tous [0;3] 4 trans_gene ;;

(* Convertir un AFD réduit en expression régulière *)
let expression_reguliere etat_initial etat_final graphe_reduit =
	let tete = transitions_etat etat_initial graphe_reduit in
		let r = fst (List.find (fun (x,y) -> y = etat_initial) tete) in
			if (etat_initial = etat_final) then Etoile(r)
			else let queue = transitions_etat etat_final graphe_reduit in
				let s = fst (List.find (fun (x,y) -> not (y = etat_initial)) tete) in
					let t = fst (List.find (fun (x,y) -> y = etat_final) queue) in
						let u = fst (List.find (fun (x,y) -> not (y = etat_final)) queue) in
				Concat(Concat(Etoile(r), s), Etoile(Ou(t,Concat(u, Concat(Etoile(r), s))))) ;;

let er03 = expression_reguliere 0 3 trans_reduit ;;

expr_en_chaine er03 ;;




(* FINAL : Donner l'expression régulière d'un AFD initial *)
(* Fonction annexe : Renvoyer une liste avec tous les AFD réduits en fonction de l'état final *)
let rec liste_afd_reduits nb_etats etat_initial etats_finaux transitions =
	match etats_finaux with
		[] -> []
		| x::r -> let p = (x, eliminer_tous ([etat_initial;x]) nb_etats transitions) in p::(liste_afd_reduits nb_etats etat_initial r transitions)
;;

liste_afd_reduits 4 0 [3;2] trans_gene ;;
(* DOnne : Couples (etat final, afd réduit) *)



let rec afd_en_expr_reg_ liste etat_initial =
	match liste with
		[] -> Vide
		| (f, x)::r -> Ou((expression_reguliere etat_initial f x), afd_en_expr_reg_ r etat_initial)
;;


let afd_en_expr_reg nb_etats etat_initial etats_finaux transitions =
	let afd_gen = generaliser_afd transitions in
		let liste_afds = liste_afd_reduits nb_etats etat_initial etats_finaux afd_gen in afd_en_expr_reg_ liste_afds etat_initial
;;


afd_en_expr_reg 4 0 [3] trans ;;

