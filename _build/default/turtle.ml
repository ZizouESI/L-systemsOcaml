open Graphics;;
open Float;;
open Formes;;

type command =
| Line of int
| Move of int
| Turn of int
| Store
| Restore

type position = { 
	x : float; (*position (x,y) et l'angle a et la couleur c*)
	y : float;
	a : int; 
	c: color; (*extension couleur*)
};;

	

(** Put here any type and function implementations concerning turtle *)

(**
	init_position : la fonction qui initailise la position de la tortue
	entrées: position (x,y) et l'angle a et la couleur c
	sorties: tuple de position de la tortue
*)
let init_position : float -> float -> int -> color -> position = (fun x y a c -> 
	{x;y;a;c}
);;

(**
	reset_position : la fonction qui réinitialise la position sur le plan (x,y) de la tortue
	entrées: tuple (position)
	sorties: unit
*)
let reset_position : position -> unit = (fun record -> 
	let x = int_of_float record.x in
	let y = int_of_float record.y in
	moveto x y ;
);;

(**
	move: la fonction avec laquelle on se déplace de n unités
	entrées: position (x,y) et angle a et n
	sorties: nouvelle position (x,y)
*)
let move : int -> int -> int ->int -> int * int =(fun x y a n ->
	let n= float_of_int n in
	let angle= (((float_of_int a) +. 90.) *. pi) /. 180. in
	x + int_of_float (n *. (cos angle)) , y + int_of_float (n *. (sin angle))
);;

(**
	execute_command : la fonction avec laquelle on execute la commande 
	entrées : position , commande 
	sorties : position * geoobject (objet geomitrique)
*)
let execute_command : position -> command -> position * geobject = (fun p c ->
	match c with
	|Move n -> 
			   let x , y = move (int_of_float p.x) (int_of_float p.y) p.a n  in 
			   let a=p.a and c=p.c in
			   let x'=float_of_int x in
			   let y'=float_of_int y in
			   {x=x';y=y';a;c},Nil
	|Line n ->
			   let x , y = move (int_of_float p.x) (int_of_float p.y) p.a n  in
			   let a=p.a and c=p.c in
			   let x'=float_of_int x in
			   let y'=float_of_int y in
		       {x=x';y=y';a;c}, Ligne (Point (int_of_float p.x,int_of_float p.y) ,Point (x,y))
	|Turn ang -> 
		let new_a=p.a + ang in
		{p with a=new_a},Nil
	|Store -> p,Nil
	|Restore -> p,Nil
);;


(**
	executer_list_command : la fonction avec laquelle on va executer une suite de commandes
	entrées : position et une liste de commande
	sorties : nouvelle position après execution de commande et un dessin (liste des objets géométriques)
*)

let rec executer_list_command p lc =
	match lc with 
	|[] -> p,init_dessin
	|x::l' ->
		let p' , geo = execute_command p x in
		let p'' , dess = executer_list_command p' l' in
		p'' ,ajouter_dessin dess geo
;;

