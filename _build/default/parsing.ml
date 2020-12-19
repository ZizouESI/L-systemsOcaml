open List ;;
open Str;;

(**
	input_line_opt : fonction pour lire un ligne de chaine de caractères d'un canal 
	entrées: canal 
	sorties: ligne de chaine de caractères s'elle existe si non None
*)
let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

(**
	read_lines : fonction pour lire les lignes d'un fichier 
	entrées : canal
	sorties : liste de chaines de caractères du fichier 
*)  
let read_lines ic =
  let rec aux acc =
    match input_line_opt ic with
    | Some line -> aux (line::acc)
    | None -> (List.rev acc)
  in
  aux []
  
(**
	lines_of_file : fonction pour lire un fichier
	entrées : nom du fichier 
	sorties : lignes du fichier obtenu par appel à read_lines
*)
let lines_of_file filename =
  let ic = open_in filename in
  let lines = read_lines ic in
  close_in ic;
  (lines)
  
(**
	explode : fonction qui prend une chaine de caractère et renvoie les caractères qui la représente sous forme de liste
	entrées : chaine de caractères
	sorties : liste de caractères
*)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [] ;;

(**
	symbols_from_list : fonction qui calcule les symbols (char's) d'une liste de chaine de caractères
	entrées : liste de chaine de caractères
	sorties : liste de caractères (symbols)
*)  
let symbols_from_list l =
  let list_chars= List.flatten (List.map (fun x -> explode x) l) in 
  List.sort_uniq compare list_chars ;;

(**
	string_of_chaine_parenthesee : fonction qui calcule une chaine paranethesee sous forme 's word 
	entrées: chaine de caractères (parenthesee)
	sorties: chaine de caractères sous format 's word de la chaine en entrées 
	exemple : string_of_chine_parenthesee A[BA]A -> Seq [Symb A ; Branch([Symb B ; Symb A]) ; Symb A]
*)
let string_of_chaine_parenthesee cp =
	let tmp=List.map (fun x -> Char.escaped x) (explode cp) in
	let rec cat l =
		match l with 
		|[]-> ""
		|x::l' -> match x with
				  |"[" -> "Branch (Seq ["^cat l'
				  |"]" -> "]) ; "^cat l'
				  |_ -> "Symb "^x^" ; "^cat l'
	in 
	"Seq ["^cat tmp^"]"

(**
	exist : fonction utilitaire qui verifie si s existe dans la liste s 
*)
let rec exist l s=	
	match l with 
	|[]->false
	|x::l'-> (x=s) || exist l' s
;;

(**
	generation_str_ml :fonction qui calcule la chaine de caractèresqui représente le code Ocaml du l-system pour l'utiliser par la suite 
	par les fonctions pour dessiner le l-system
	entrées : axiom : chaine de caractères ,  rules , interpretation : liste de chaine de caractères 
	sorties : chaine de caractères (en syntaxe ml)
*)
let generation_str_ml axiom rules interp =
	let symbols=symbols_from_list ((axiom::rules)@interp) in
	let symbols_list_str=List.map (fun x -> Char.escaped x) symbols in
	let r= regexp "\\([A-KM-QU-Z]+\\)" in
	let symbols=List.filter (fun x ->string_match r x 0) symbols_list_str in
	let s1="open Graphics;;\nopen Turtle;;\n" in
	let rec str_list_str l sep len =
		match l with 
		|[]->""
		|[x] -> x
		|x::l'-> x^sep^(str_list_str l' sep (len-1))
	in
	let s2= "type symbol = "^(str_list_str symbols "|" (List.length symbols))^"\n" in
	let list_symb= "let list_symb =["^(str_list_str symbols ";" (List.length symbols))^"]\n" in 
	let declarations ="type 's word =\n\t\t| Symb of 's\n\t\t| Seq of 's word list\n\t\t| Branch of 's word\ntype 's rewrite_rules = 's -> 's word\ntype 's interpretation = 's -> Turtle.command list\ntype 's system = {\n\t\taxiom : 's word;\n\t\trules : 's rewrite_rules;\n\t\tinterp : 's -> Turtle.command list\n}\n" in
	let s3= "let sys : symbol system =\n{\n" in
	let concerner_par_rules = List.map (fun x -> x.[0]) rules in
	let rec make_rules l =
		match l with 
		|[]->""
		|[x] when (exist concerner_par_rules x.[0]) -> "| "^Char.escaped x.[0]^" -> "
				^string_of_chaine_parenthesee (String.sub x 2 ((String.length x)-2))
				^"\n"
		|x::l' when (exist concerner_par_rules x.[0]) 
				-> "| "^Char.escaped x.[0]^" -> "
				^string_of_chaine_parenthesee (String.sub x 2 ((String.length x)-2))
				^"\n\t\t"^(make_rules l')
		|_ -> ""
	in
	let s4="axiom = "^string_of_chaine_parenthesee axiom^";\n" in
	let s5="rules = \n\t\t(function \n\t\t"^make_rules rules^"\t\t| s -> Symb s\n\t\t);\n" in
	let rec make_interp l =
		match l with 
		|[]->""
		|x::l' -> match x.[2] with
				|'L'->"| "^(Char.escaped x.[0])^" -> [Line ("^(String.sub x 3 ((String.length x)-3))^")]\n\t\t"^(make_interp l')
				|'M'->"| "^(Char.escaped x.[0])^" -> [Move ("^(String.sub x 3 ((String.length x)-3))^")]\n\t\t"^(make_interp l')
				|'T'->"| "^(Char.escaped x.[0])^" -> [Turn ("^(String.sub x 3 ((String.length x)-3))^")]\n\t\t"^(make_interp l')
				|'S'->"| "^(Char.escaped x.[0])^" -> [Store ("^(String.sub x 3 ((String.length x)-3))^")]\n\t\t"^(make_interp l')
				|'R'->"| "^(Char.escaped x.[0])^" -> [Restore ("^(String.sub x 3 ((String.length x)-3))^")]\n\t\t"^(make_interp l')
		|_ -> ""
	in		
	let s6="interp = \n\t\t(function \n\t\t"^make_interp interp^");\n}" in
	s1^s2^list_symb^declarations^s3^s4^s5^s6
	
(**
	get_axiom : fonction qui retourne l'axiome à partir de la liste lines (qui représente les lignes du fichier dont se fait la lecture 
	du système *.sys)
	entrées : lignes du fichier *.sys sous forme de liste de chaine de caractères
	sorties : chaine de carctères représentant l'axiome du l-system
*)
let get_axiom lines =
	let l=List.filter (fun x -> if (x<>"") then (x.[0] <> '#') else true) lines in
	List.hd l ;;

(**
	get_rules : fonction qui retourne les substitutions à appliquer à partir de la liste lines 
	entrées : lignes du fichier *.sys sous forme de liste de chaine de caractères 
	sorties : liste de chaine de caractères représentant les substitutions du l-system
*)
let get_rules lines =
	let l=List.filter (fun x -> if (x<>"") then (x.[0] <> '#') else true) lines in
	let l'=List.tl (List.tl l) in
	let rec find_index l =
		match l with 
		|[]->0
		|x::l' -> if (x <> "") then 1+ find_index l' else 1
	in
	let i=((find_index l') -2) in
	let rec append acc l index =
		match l with 
		|[] -> acc
		|x::l'' -> if (index >= 0) 
		then append (List.rev (x::acc)) (List.tl l) (index-1) 
		else acc
	in
	append [] l' i 
;;
(**
	get_interp : fonction qui retourne les interpretations de chaque symbol du l-system
	entrées: lignes du fichier *.sys
	sorties : liste de chaine de caractères représentant les interpretations du l-system
*)
let get_interp lines =
	let l=List.filter (fun x -> if (x<>"") then (x.[0] <> '#') else true) lines in
	let l'=List.tl (List.tl l) in
	let rec find_index l =
		match l with 
		|[]->0
		|x::l' -> if (x <> "") then 1+ find_index l' else 1
	in
	let i=find_index l' in
	let rec reduce l index =
		if index = 0 then 
			l
		else 
			reduce (List.tl l) (index-1)
	in
	reduce l' i
;;

(**
	create_ml_file : la fonction avec laquelle on crée le fichier mysys.ml qui va contenir le l-système à dessiner (en code ml)
	entrées : chaine de caractère du code ocaml du l-system 
	sorties : unit (création du fichier mysys.ml)
*)
let create_ml_file str =
	let c=open_out "mysys.ml" in
	output_string c str;
	close_out c 
;;

(**
	recup_system : fonction qui utilise les fonctions décrite en dessus pour créer le fichier mysys.ml
	entrées : lignes du fichier *.sys
	sorties : code ocaml représentant le l-system décrit dans le fichier *.sys
*)
let recup_system lines =
	let axiom = get_axiom lines in (*axiom :string*)
	let list_rules = get_rules lines in (*rules : string list*)
	let list_interp = get_interp lines in (*interp : string list*)
	(*génération du fichier *.ml correspondant au système décrit dans *.sys *)
	let str = generation_str_ml axiom list_rules list_interp in
	create_ml_file str
;;	
		
(*main*)
let () = 
	let lines = lines_of_file Sys.argv.(1) in
	recup_system lines
	
