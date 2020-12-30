type graphe = (int* int list) list ;;

let g = [ (1,[6;7;8]); (2,[1;4]) ; (3,[2]); (4,[3;5]); (5,[1]); (6,[5;7]) ;(7,[]); (8,[6;7])];;

(*1/ Liste de tous les sommets*)
(* Cette function prend un graphe comme parametre et retourne une liste des sommets*)
let rec sommet g = match g with
 	[] -> []
	|(s,suc)::r -> s::sommet r ;;

(*2/ Liste des successeurs d'un sommet donnée*)
(* Cette function prend un graphe et un sommet comme parametre et retourne une liste des successeurs de ce sommet *)
(* On parcours le graphe si le sommet = le sommet passé en parametre => on retourne ses successeurs*)

let rec list_successeurs som g = match g with
	[] -> []
	|(s,suc)::r-> if s = som then suc
				else list_successeurs som r ;;


(*3/ Inserver le graphe*)

(*Cette function prend graphe comme parametre et elle supprime la liste des successeurs*)
let rec supprime_succ g = match g with
	[] -> []
	| (s,succ)::r -> (s,[])::(supprime_succ r);; 

(*Cette function cree un liste de liste de taille a*)
let rec cree_ll a = match a with
	0 -> []
	|n -> []::(cree_ll (n-1));;

(*Cette function va ajouter les successeurs dans un liste de liste bien à sa position *)
let rec ajouter s succ ll = match (succ,ll) with
	_,[] -> []
	|n,l::r -> if n = 1 then (s::l)::r 
				else l::(ajouter s (succ-1) r) ;;


let rec ajouter_s_succ som succ ll = match succ with
 	[] -> ll
	|x::r -> ajouter_s_succ som r (ajouter som x ll);;


(*Cette function va ajouter le sommet dans la liste de liste*)
let rec ajouter_som g ll = match (ll,g) with
	[],_ -> []
	|_,[] -> []
	|l::r,(s,succ)::gr -> (s,l@succ)::(ajouter_som gr r);;


(*La function inverser*)
(*Explication : *)
(* liste ll = [ []; []; []; []; []; []; []; []*)
(*match graphe => pour chaque element dans le graphe on applique la function *)
(* ajouter_s_succ 1 [2;3] ll => ajouter_s_succ 1 [3] (ajouter 1 ll)*)
(*ca va donner ll2 = [ []; [1]; [1];...] ]*)
(*une fois on ajouter les successeurs dans la liste ll2 bien a sa place*)
(*on va ajouter les sommets*)
let rec inverser g = let ll = cree_ll (List.length g)
							in match g with
							[] -> []
							|(s,succ)::r -> let ll2 = List.fold_left (fun n x -> match x with
																	(s,[]) -> ajouter_s_succ s [] n
																	|s,succ -> ajouter_s_succ s succ n 
																) ll g
											in ajouter_som (supprime_succ g) ll2;;


(*4/ Parcours en profondeur*)

(*Cette function va tester un element appartient à la liste ou non*)
let rec appartient elt l = match (elt,l) with
	_,[] -> false
	| x,y::r -> if y = elt then true
			else appartient elt r ;;

(*Cette function va marquer(enregistrer) element elt dans la liste l*)
let rec marquer elt l = if not (List.mem elt l) then elt::l 
						else l ;; 

(*La function parcours en profondeur*)
(*Explication : On parts de sommet 1 (sommet de base) on regarde si sa liste des successeurs est vide ou non ?*)
	(* Si elle n'est pas vide. On va regarder sa liste de successeurs. On va regarder encore ses successeurs de ce 1er element dans cette liste*)
		(*Si sa liste de ses successeurs n'est pas vide on regarde encore les successeurs de 1er element dans cette liste*)
		(*Quand un de ses successeurs = le sommet de base on va marquer son sommet *)
	(* Si *elle est vide on va marquer son sommet et retourne ver le sommet initial*)

(*Déroulement*)
(* Sommet de base = 1 | sommet courant = 1*)
(* (1[6;7;8] => (6[5;7]) => (5[1]) | 1 = sommet de base => marquer 5 dans une liste*)
(*(6[5;7]) 5 est dans liste donc on passe le 7. (7[]) | liste de ses successeur est vide donc marquer 7 dans la liste*)

let parcour_profondeur graphe = 
	let rec parcour_succ sommetBase sommmetCourant succCourant listeExplore graphe = match succCourant with
		[] -> if sommetBase == List.length (sommet graphe) then listeExplore
			  else if sommetBase == sommmetCourant then parcour_succ	(sommetBase+1)	(sommetBase+1) (list_successeurs (sommetBase+1) graphe) (marquer sommetBase listeExplore) graphe
			  else	parcour_succ sommetBase	sommetBase (list_successeurs sommetBase graphe) (marquer sommmetCourant listeExplore) graphe
		| x::r -> if appartient x listeExplore then parcour_succ sommetBase sommmetCourant r listeExplore graphe
 			 else if not(appartient x listeExplore) && x != sommetBase then parcour_succ sommetBase x (list_successeurs x graphe) listeExplore graphe 
			 else if not(appartient x listeExplore) && x = sommetBase then 
											parcour_succ sommetBase 1 (list_successeurs sommetBase graphe) (marquer sommmetCourant listeExplore) graphe				 
			else failwith "Erreur"
	in parcour_succ 1 1 (list_successeurs 1 graphe) [] graphe;;



(*5/ Rechercher les composants fortement connexes*)
(*Cette function va retourner le graphe à partir le sommet donné*)
let rec next graphe n =
	match graphe,n with 
	|[],_-> graphe
	|(i,_)::r,n-> if n == i then graphe
					else next r n ;;


(*Cette function va ajouter une liste dans une liste*)
(*idée de cette function est de permetre de creer une liste comme ca  [ [x]; [y]] pour la question 5*)
let rec ajouter_ll l1 l2 = 
	let rec f l l1 l2 = match l1,l2 with
	|[],[] -> l
	|l1,[] -> l1::l
	|l1,l2 -> f (l2::[]) l1 []	
in f [] l1 l2 ;;


(*Cette function retourner les boucle lors qu'on fait un parcours en profondeur sur le graphe inverse *)
let rec boucle graphe liste = match graphe with
	[] -> liste 
	|(_, [])::[] -> liste
	|(s, [])::r -> boucle r (marquer s liste)
	|(s,y::x)::r -> 	if not(appartient s liste)  then boucle  (next graphe y) (marquer s liste) 
						else if appartient s liste then boucle [] liste
						else failwith "Erreur" ;; 

(*Cette function va chercher les boucles lors qu'on fait un parcours en profondeur sur tout le graphe*)
let rec boucle2 graphe liste = 
	match graphe with
	[] -> liste
	|(n,succ)::r -> boucle r  ((boucle graphe [])@liste) ;;



let fortement_connexes graphe =
	let rec inv graphe liste l_inv = 
		match graphe with
		|[]-> l_inv
		|(n, [])::r -> inv r liste (n::l_inv)
		|(_,h::t)::r-> if List.length l_inv = List.length liste 	then inv [] liste l_inv 
				else if  List.mem h l_inv then inv r liste l_inv
				else inv r liste ((boucle graphe  [])@l_inv) 				
	in inv graphe (parcour_profondeur graphe) [] ;;


g;;
list_successeurs 4 g;;
inverser g;;
parcour_profondeur g;;

let g_inv = inverser g;;
boucle2 g_inv [];;


(*partie 2 *)

(*1*)
(*Ici jai compris la question 1: Il faut retourner le sommet et le plus petit successeur dans la liste de ses successeurs*)
(*Cette function prend une liste comme parametre et retourne le minimum dans cette liste*)
let rec min_liste li = match li with
	[] -> failwith "liste vide"
	| [x] -> x
	| x::r -> let n = min_liste r
				in if x < n
					then x
				else n;;

(*Cette function va retourner le coule sommet s et son petit successeur*)
let rec info_noeud g = match g with
[] -> []
| (s,succ)::r -> match succ with
			[] -> s::[]
			|[x] -> s::(x::[])
			|x::y -> s::((min_liste succ)::[]) ;;

(*Cette function va retourner le coupe sommet s et son petit successeur pour tout le graphe*)
let rec info_noeud2 g l= 
	match g with
	|[] -> l
	|(s,succ)::r -> info_noeud2 r (info_noeud g) ;;



(*2 Avec une pile*)

let rec succ_de = function x -> function gr-> match gr with
	[] -> []
	|(s,succ)::reste ->  if s = x then succ
						else succ_de x reste;;

let rec succ_marque liste ll = match liste with 
							[]-> true
							| x::r-> List.mem x ll && succ_marque r ll;;

let rec successeur  i pile succ = match succ with
							   [] -> 0
							   |x::r ->  if List.mem x pile then successeur i pile r 
										else x ;; 

let rec bloque x pile g = if succ_de x g = [] || succ_marque (succ_de x g) pile || successeur x pile (succ_de x g)=0 then true
						   else false;;

let tete pile = match pile with
				[]-> failwith " pile vide"
				|x::r -> x;;

let depile pile = match pile with
				[]-> []
				|x::r -> r;;
let empile x pile = if List.mem x pile then pile 
					else x::pile;;
let rec node i g =  match g with
								 [] -> failwith "graphe vide"
								| (s,succ)::reste ->	if s = i then (s,succ)
														else node i reste;;

let rec s x g = match g with 
					[] -> []
					| (y,succ)::reste -> if List.mem x succ then y::(s x reste)
											else s x reste;;

let rec pre_Node  x pile vue s= match pile with
								[] ->x+1
								|x::r->     if List.mem x s && not(List.mem x vue) then x
										    else  pre_Node x r vue s;;	

