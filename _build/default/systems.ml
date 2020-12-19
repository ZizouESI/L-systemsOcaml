open Turtle;;
open Formes;;
open List;;
open Char;;
open Graphics;;
open Mysys;;



type list_tuple_rules = (symbol * symbol word) list ;;

type list_tuple_interpretation = (symbol * command list) list ;;

   


(*symbol list -> symbol rewrite_rules -> list_tuple_rules *)
let transform_rules srr =
	let list_s= Mysys.list_symb in
	map (fun x -> (x,(srr x))) list_s
;;
(*symbol list -> symbol interpretation -> list_tuple_interpretation *)
let transform_interp i =
	let list_s= Mysys.list_symb in
	map (fun x -> (x,(i x))) list_s
;;

(**
	rewrite_symbol : fonction qui fait la réecriture d'un symbol en suivant les substitutions
	entrées : symbol et règles de substitutions
	sorties : symbol word
*)

let rewrite_symbol : symbol -> list_tuple_rules -> symbol word =(fun s rr ->
 fold_left (fun acc (symb , res) -> if s=symb then res else acc ) (Symb s) rr
 );;
 
(**
	rewrite_word : fonction qui fait la réecriture d'une chaine parenthesee en suivant les règles de substitutons
	entrées : word et symbol
	sorties : symbol word
*) 
let rec rewrite_word w rr =
	match w with 
	|Symb s -> rewrite_symbol s rr 
	|Seq seq -> Seq (map (fun elt -> rewrite_word elt rr) seq)
	|Branch b -> Branch (rewrite_word b rr)
;;

(**
	iterer : la fonction avec laquelle on calcule les itération du l-system pour le dessiner
	entrées : nombre d'itérations et mot (word) et les régles de substitutions 
	sorties : symbol word 
*)
let rec iterer n w rr = 
	if n <= 0 then
		w
	else
		iterer (n-1) (rewrite_word w rr) rr 
;;

(**
	interpreter_symbol : la fonction avec laquelle on interprète les symboles
	entrées : symbol et les interprétations
	sorties : list de commande 
*)
 	
let interpreter_symbol :symbol -> list_tuple_interpretation -> command list =(fun  s c ->
	fold_left (fun acc (symb , res) -> if s= symb then res else acc ) [] c
);;

(**
	interpreter : la fonction avec laquelle on interprète les mots 
	entrées : word , une position (on commence par init_position) , interpretation 
	sorties : une position et un dessin
*)
let rec interpreter w pos inter = 
	match w with 
	|Symb s -> executer_list_command pos (interpreter_symbol s inter)
	|Seq seq -> fold_left (fun (position , dessin) elt -> 
							let p , d = interpreter elt position inter in 
							(p , dessin @ d ) 
						  ) (pos , init_dessin) seq 
	|Branch b -> let _ , d = interpreter b pos inter in 
				 pos, d
;;

(**
	dessin_expression : la fonction avec laquelle on calcule les dessin d'une expression (elle utilise interpreter)
	entrées : word , position , interpretation 
	sorties : dessin 
*)
let dessin_expression w p inter =
	let _,d = interpreter w p inter in
	d
;;

(**
	draw_system : la fonction avec laquelle on dessine le l-system 
	entrées : system , interpretation , alpha_init (paramètre donnée par l'utilisateur) , color (paramètre donnée par l'utilisateur)
	sorties : dessin graphique du l-system
*)
let draw_system system inter alpha_init color=
	let border = 30 in
	let drawing = dessin_expression system (init_position 0. 0. alpha_init color) inter in
	let minx,maxx,miny,maxy = extremums_dessin drawing in
	let window_width = maxx - minx + 2*border in
	let window_height = maxy - miny + 2*border in
	let centered_drawing = map_dessin ( fun (x,y) ->
		(x-minx+border, y-miny+border)
	) drawing in
	set_color color;
	resize_window window_width window_height;
	dessiner centered_drawing;	
;;	
(**
	lsystem : fonction qui ouvre la fenêtre graphique et appel la fonction de dessin draw_system 
	entrées : 
	sorties : 
*) 
let lsystem init hered inter n alpha color =
	let system = iterer n init hered in
	open_graph "";
	draw_system system inter alpha color;
	ignore (wait_next_event [Button_down]);
	close_graph ()				 
;;
