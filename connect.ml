let rec aller_a graphe n =
	match graphe,n with 
	|[],_-> graphe
	|(i,_)::r,n-> if n == i then graphe
					else aller_a r n ;;


let rec cycle graphe liste = match graphe with
	[] -> liste 
	|(_, [])::[] -> liste
	|(s, [])::r -> cycle r (marquer s liste)
	|(s,y::x)::r -> 	if not(appartient s liste)  then cycle  (aller_a graphe y) (marquer s liste) 
						else if appartient s liste then cycle [] liste
						else failwith "Erreur" ;; 


let rec ajouter_ll l1 l2 = 
	let rec f l l1 l2 = match l1,l2 with
	|[],[] -> l
	|l1,[] -> l1::l
	|l1,l2 -> f (l2::[]) l1 []	
in f [] l1 l2 ;;


let rec cycle2 graphe liste = 
	match graphe with
	[] -> liste
	|(n,succ)::r -> cycle r  ((cycle graphe [])@liste) ;;
