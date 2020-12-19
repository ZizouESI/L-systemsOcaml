open List;;
open Graphics;;

(*définition du type point*)
type point =Point of int * int;;

(*définition du type geobject , qui est un objet géométrique *)
type geobject =
	|Nil
	|Ligne of point * point ;;
	
(*défintion du type dessin*)
 type dessin = geobject list;;
 
(**
	geobject_to_string : la fonction qui fournie la représentation d'un objet geométrique 
	sous forme d'une chaine de caractères
*)
let geobject_to_string g = 
	match g with 
	|Nil -> "Nil"
	|Ligne (Point(x,y),Point(u,v))->
		let x= string_of_int x in
		let y= string_of_int y in
		let u= string_of_int u in
		let v= string_of_int v in
		"(" ^ x ^ "," ^ y ^ ") - (" ^ u ^ "," ^ v ^ ")"
;;

(**
	init_dessin : initialiser un dessin
*)
let init_dessin : dessin = [] ;;
(**
	ajouter_dessin : ajout d'un objet geo à la liste des dessin 
*)
let ajouter_dessin : dessin -> geobject -> dessin = (fun d g ->
	match g with 
	|Nil -> d
	|Ligne _ -> g::d
);;

(**
	dessiner : la fonction avec laquelle on dessine un objet géométrique 
	entrées : dessin à dessiner 
	sorties : unit (void)
*)
let dessiner : dessin -> unit = (fun d -> 
	iter (fun g ->
		match g with 
		|Nil -> ()
		|Ligne (Point (x,y) ,Point(u,v)) -> moveto x y ; lineto u v ;
		) d 
);;

(**
	extremums_dessin : la fonction avec laquelle on calcule les dimensions maximales et minimales 
	de dessin 
	entrées : dessin 
	sorties : quadruplet x, u , y, v
*)
let extremums_dessin : dessin -> int *int *int *int =(fun d ->
	let points = fold_left (fun acc g ->
						match g with 
						|Nil -> acc
						|Ligne (p1,p2) -> p1 :: p2 :: acc 
						) [] d in
	let Point (x,y) = hd points in
	fold_left (fun acc p ->
				let x,u,y,v = acc in
				let Point (xp,yp) = p in
				min x xp , max u xp , min y yp , max v yp
			  ) (x,x,y,y) (tl points)
);;

(**
	taille_dessin : la fonction qui calcule la taille du dessin 
	entrées : dessin 
	sorties : tuple de taille (int * int)
*)		
let taille_dessin : dessin -> int * int = (fun d ->
 	let x,u,y,v = extremums_dessin d in 
 	u - x , v - y
 );;
 
 (**
 	map_dessin : une fonction (FOS) qui applique une autre "f" pour la liste des objets geométriques (dessin)
 	entrées : fonction f 
 	sorties : dessin
 *)
 let map_dessin f (d : dessin) = 
	map ( fun g ->
			match g with 
			| Nil -> Nil
			| Ligne (Point(x,y),Point(u,v)) -> 
							let x,y = f (x,y) in
							let u,v = f (u,v) in
							Ligne (Point(x,y),Point(u,v))
		) d
;;
 
 
