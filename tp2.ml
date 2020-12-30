(** longeur **)

let rec longeur_list li = match li with
	[] -> 0
	| x::r -> 1 + longeur_list r;;


let rec longeur = function 
	[] -> 0
	| x::r -> 1 + longeur r;;

(** concat **)

let rec concat li_1 li_2 = match (li_1,li_2) with
  	[],[] -> failwith "2 listes sont vides"
  	|[], x::r -> li_2
  	| x::r, [] -> li_1
  	| x1::r1,x2::r2 -> x1::concat r1 (x2::r2);;


let rec concat = function li_1 -> function li_2 ->
	match (li_1,li_2) with
  	[],[] -> failwith "2 listes sont vides"
  	|[], x::r -> li_2
  	| x::r, [] -> li_1
  	| x1::r1,x2::r2 -> x1::concat r1 (x2::r2);;

(** nieme **)

let rec nieme li n = match (li,n) with
	[],_ -> failwith "liste vide"
	| x::r,1 -> x
	| x::r,n -> nieme r (n-1);;


let rec nieme = function li -> function n ->
	match (li,n) with
	[],_ -> failwith "liste vide"
	| x::r,1 -> x
	| x::r,n -> nieme r (n-1);;


(** npremier **)

let rec npremiers li n = match (li,n) with
	[],_ -> failwith "pas assez elet"
	|x::r,1 -> x::[]
	| x::r,n -> x::npremiers r (n-1);; 

(** met a plat **)

let rec met_a_plat l = match l with
	 [] -> []
	| x::r -> x@met_a_plat r;;

(** supprime 1 **)

let rec supprime_1 x l = match (x,l) with
	_,[] -> []
	|x,y::r -> if y = x
			then r
			else supprime_1 x r ;;

let rec supprime_2 x l = match (x,l) with
	_,[] -> []
	|x,y::r -> if y != x
			then y::supprime_2 x r
			else supprime_2 x r ;;

let rec min_liste li = match li with
	[] -> failwith "liste vide"
	| [x] -> x
	| x::r -> let n = min_liste r
				in if x < n
					then x
				else n;;

let rec doublon li = match li with
	[] -> [] 
	|x::r -> 


(**Ex 3 **)

let inser x l = match (x,l) with
	x,[] -> x::[]
	|x,y::r-> x::y::r ;;

let inserer x ll = 
	List.map (inser x) ll ;;


let parties l = 
	List.map 




let puissance x = match x with
| 0 -> failwith "Erreur"
| x -> x*x;;


let puissance = function
	0 -> failwith "Erreur"
	| x -> x*x;;

let vide = function
	[] -> true
	|_ -> false;;

let vide2 l = match l with
	[] -> true
	|_ -> false;;

let rec mini li = match li with
| [] -> failwith "vide"
| [x] -> x
| x::r -> let n = mini r in 
						if x < n
							then x
						else n;;

let rec mini2 = function
| [] -> failwith "vide"
| [x] -> x
| x::r -> let n = mini r in 
						if x < n
							then x
						else n ;;

let rec trouver_tous p li = match li with
	[] -> []
	| x::r -> if p x
			then x:: trouver_tous p r
			else trouver_tous p r ;;

let rec appartient x li = match li with
	 [] -> false
	| y::r -> if x=y
			then true
		else appartient x r ;;

let appartient_ssliste x ll = 
	trouver_tous (appartient x) ll ;;



let supprime x l =
	trouver_tous (fun e -> e != x) l;;

let supprime_ssliste x ll =
	List.map (supprime x) ll;;

let supprime_ssliste_2 x ll =
	List.map (fun l -> supprime x l) ll ;;

let supprime_ssliste_3 x ll =
	List.map (trouver_tous(fun e -> e != x)) ll;;




let somme n x = x+n;;

let somme_l l = 
	List.fold_left somme 0 l;;

let somme_l_2 l =
	List.fold_left ((fun n x -> x+n)) 0 l;;


let car x = x*x;; 
let carre l = 
	List.map car l;;


let somme_long ll =
	List.fold_left (fun n l -> n + List.length l) 0 ll;;

let long n l = n + List.length l;;

let somme_long_2 ll =
	List.fold_left long 0 ll;;




let long_ l =
	List.fold_left (fun n x -> n+1) 0 l;;

(**

let boucle2 graphe = 
	let rec f l graphe = match graphe with
	[] -> l::[]
	|(n,succ)::r -> f (ajouter_ll (boucle r []) l ) r 
in f [] graphe ;;




let boucle2 graphe =
	let rec f acc = function
	|[] -> acc
	|(n,succ)::r -> boucle r [] ;; 
in f [] graphe ;;   

**) 



let rec getNoeud som g = match g with
	[] -> []
	|(s,succ)::r -> if s = som then (s,succ)::[]
					else getNoeud som r;; 