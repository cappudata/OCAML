(**Exercice 1 **)

(**#1**)
(*
type 'x arbre = Feuille of 'x | Noeud of ('x arbre * 'x arbre);;

(**#2**)
let arbre = Noeud(Noeud(Feuille 1, Feuille 2),Noeud(Feuille 3, Feuille 4));;
let a1 = Noeud(Feuille 9 , Noeud(Feuille 5 , Noeud(Feuille 3, Feuille 4)));;


(**#3**)
let rec nbr_noeuds a = match a with
	 Feuille _ -> 0
	| Noeud(a1,a2) -> 1 + (nbr_noeuds a1) + (nbr_noeuds a2) ;;

(**#4**)
let rec nbr_feuille a = match a with
 	Feuille _ -> 1
	| Noeud(a1,a2) -> (nbr_feuille a1) + (nbr_feuille a2) ;;

(**#5**)

let profondeur a = match a with
 	Feuille _ -> 0
	| Noeud(a1,a2) -> 1 + max (nbr_noeuds a1) (nbr_noeuds a2);;

let max x1 x2 = 
	if x1>x2
		then x1
	else x2 ;;

(**#6**)
 let rec test_arbre a1 a2 = match (a1,a2) with
 	 Feuille _, Feuille _ -> true
 	| Noeud(sa1,sa2), Noeud(sa3,sa4) -> (test_arbre sa1 sa3) && (test_arbre sa2 sa4)
 	| Feuille _, Noeud(sa3,sa4) -> false
 	| Noeud(sa1,sa2), Feuille _ -> false

(**#7**)

let rec arbre_liste a = match a with
	 Feuille x  -> x::[]
	| Noeud(a1,a2) -> arbre_liste a1 @ arbre_liste a2;;

(**#8**)

let map_arbre a = match a with
| patt -> expr
| _ -> expr2


(**exo2**)

type operateur_bin = Mult | Add ;;
type operateur_un = Moins ;;
type arbre = Const of int | Var of string | Noeud1 of (operateur_un * arbre) | Noeud2 of (operateur_bin * arbre * arbre) ;;

let a = Noeud2(Mult,Var "x", Const 3);;

let rec chaine_de_arbre a = match a with
	(Const i)-> string_of_int i
	|(Var s) -> s
	|Noeud1(Moins,a1) -> "(-"^chaine_de_arbre a1^")"
	|Noeud2(Mult,sa1,sa2) -> "("^chaine_de_arbre sa1^"*"^chaine_de_arbre sa2^")"
	|Noeud2(Add,sa1,sa2) -> "("^chaine_de_arbre sa1^"+"^chaine_de_arbre sa2^")" ;;



let rec close e l = match (e,l) with
 	_,[] -> false
	|Var c ,(nom,valeur)::r -> if c = nom then true
							else close (Var c) r
	|Noeud1(Moins,a1),(nom,valeur)::r -> close a1 l
	|Noeud2(_,sa1,sa2),(nom,valeur)::r -> close sa1 l && close sa2 l
	|Const c,_ -> true;;


let rec eval e l = if close e l then match (e,l) with
	Var y, (nom,valeur)::r -> if y = nom then valeur
								else eval (Var y) r
	|_,[] -> 0
	|Const c,_ -> c
	|Noeud1(Moins,a1),(nom,valeur)::r -> -(eval a1 l)
	|Noeud2(Mult,sa1,sa2),(nom,valeur)::r -> (eval sa1 l) * (eval sa2 l)
	|Noeud2(Add,sa1,sa2),(nom,valeur)::r -> (eval sa1 l) + (eval sa2 l)
else failwith "Erreur" ;;

*)
(**Exo3**)


type operateur = Mult | Plus | Moins ;;
type arbre = C of int | N of (operateur * arbre list);;

let b = N(Moins,[C 2; N(Moins,[C 4;C 1])]) ;;
let c = N(Mult,[C 2; N(Plus,[C 6;N(Mult,[C 2; N(Plus,[C 2;C 3])]);N(Mult,[C 2; N(Plus,[C 2;C 3])])])]) ;;

let rec nbr_cst a = match a with
 C c -> 1
| N(_,[]) -> 0
| N(y,x::r) -> match x with
				 C c -> 1 + nbr_cst (N(y,r))
				 |N(y,liste)-> 0 + nbr_cst (N(y,r)) + nbr_cst (N(y,liste));; 

nbr_cst c;;
let empty n liste = match liste with
	 N(y,[]) -> false
	| N(y,x::r) -> n && true
	| C c -> n && true;;

let rec verifier a = match a with
						 C c-> true
						| N(y,liste) ->
						List.fold_left empty true liste;;
 

verifier b;;

let rec calcule a = match a with
				  C c -> c
				 | N(Plus,[])-> 0
				 | N(Plus,x::liste)-> match x with
									 C c -> c + calcule (N(Plus,liste))
									 |N(Plus,lis) -> calcule (N(Plus,liste)) + calcule (N(Plus,lis))
									 |N(Mult,lis) -> calcule (N(Plus,liste)) * calcule (N(Mult,lis))
									 |N(Moins,lis) -> calcule (N(Plus,liste)) - calcule (N(Moins,lis))

   				 | N(Moins,[])-> 0;;
				 | N(Moins,y::liste2)-> match y with
									 C q -> q - calcule (N(Moins,liste2))
									 |N(Plus,lis) -> calcule (N(Moins,liste2)) + calcule (N(Plus,lis))
									 |N(Mult,lis) -> calcule (N(Moins,liste2)) * calcule (N(Mult,lis))
									 |N(Moins,lis) -> calcule (N(Moins,liste2)) - calcule (N(Moins,lis))
		         | N(Mult,[])-> 1
				 | N(Mult,z::liste3)-> match z with
									 C p -> p * calcule (N(Mult,liste3))
									 |N(Plus,lis) -> calcule (N(Mult,liste3)) + calcule (N(Plus,lis))
									 |N(Mult,lis) -> calcule (N(Mult,liste3)) * calcule (N(Mult,lis))
									 |N(Moins,lis) -> calcule (N(Mult,liste3)) - calcule (N(Moins,lis));;

let rec calcule a = match a with
	C c -> c
	|N(Mult,x::r) -> fun x1 x2 -> x1*x2
						in 


calcule b;;



(*
type 'x arb = Vide | Arbrebin of ('x * 'x arb * 'x arb) ;;



let rec inserer a e = match a with
 	Vide -> Arbrebin(e,Vide,Vide)
	| Arbrebin (y,a1,a2) -> if e <= y
						then Arbrebin(y, (inserer a1 e),a2)
						else Arbrebin(y, a1, (inserer a2 e)) ;;

let liste_arbre l =
	List.fold_left inserer  Vide l;;

let liste_arbre_2 l =
	List.fold_left ((fun a e -> inserer a e))  Vide l;;
*)