open Lsystems (* Librairie regroupant le reste du code. Cf. fichier dune *)
open Systems ;;
open Turtle ;;
open Formes;;
open Sys;;
open Graphics;; 
open Mysys;;
open Unix;;

let n=ref 0 
let angle=ref 0
let color=ref 0x0
let nomfichier=ref ""
let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"

let action_what () = Printf.printf "%s\n" usage; exit 0
let set_n m = n:=m
let set_a l = angle:=l
let set_color c = color :=c
let set_name_file nf = nomfichier := nf
let cmdline_options = [
("-n" , Arg.Int set_n , "Le nombre d'itérations");
("-a" , Arg.Int set_a , "L'angle d'affichage de la fenêtre");
("-c", Arg.Int set_color , "Une couleur est spécifié par son R G B (R V B) , chaque composante est dans le rang de 0..255 , les trois composantes sont regroupées dans un entier int: 0xRRGGBB où RR sont les deux composantes en hexadécimal pour le rouge et GG pour le vert et le BB pour le blue");
("-f" , Arg.String set_name_file , "le nom du fichier *.sys définis le l-système");
("--what" , Arg.Unit action_what, "description");
]

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s)

let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  let init = sys.axiom in
  let hered= transform_rules sys.rules in
  let inter=transform_interp sys.interp in
  try
  	lsystem init hered inter !n !angle !color;
  with 
  | Graphics.Graphic_failure _ -> () ;;


(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = if not !Sys.interactive then main ()
