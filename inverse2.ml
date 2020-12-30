type graphe =(int*int list) list;;
let grapheEx = [(1,[6;7;8]);(2,[1;4]);(3,[2]);(4,[3;5]);(5,[1]);(6,[5;7]);(7,[]);(8,[6;7])];;
let gra = [ (1,[4]); (3,[1;5]); (4,[]); (5,[4;6]); (6,[])];;

let rec supp_les_fils gra = match gra with
						 [] -> []
						| (pere,fils)::reste-> (pere,[])::(supp_les_fils reste);; 
let rec cree_ll a = match a with 
				0 ->[]
				| n -> []::(cree_ll (n-1)) ;; 

let rec ajoute  pere fils ll = match fils,ll with
						_,[] ->[]
						|n,l::r -> if n = 1 then (pere::l)::r
									else l::(ajoute pere (fils-1) r);;   				

let rec ajout_pere_fils pere liste ll = match liste with 
						[] -> ll
						| x::reste ->  ajout_pere_fils pere reste (ajoute pere x ll);;

let rec ajoute_arc gra ll = match ll,gra with
							[],_ -> gra
							|_,[] -> []
							|l::reste,(pere,fils)::gr -> (pere,l@fils)::(ajoute_arc gr reste);;
					
let rec inverse gra = let ll = cree_ll (List.length gra)
							in match gra with
							[]->[]
							|(pere,fils)::reste -> let ll2 = List.fold_left (fun n x -> match x with 
																			 (pere,[])-> ajout_pere_fils pere [] n
																			 |pere,fils -> ajout_pere_fils pere fils n
																   ) ll gra
													in ajoute_arc (supp_les_fils gra) ll2;;

inverse gra;;
