open Graphics;;
open Turtle;;
type symbol = A|B|C
let list_symb =[A;B;C]
type 's word =
		| Symb of 's
		| Seq of 's word list
		| Branch of 's word
type 's rewrite_rules = 's -> 's word
type 's interpretation = 's -> Turtle.command list
type 's system = {
		axiom : 's word;
		rules : 's rewrite_rules;
		interp : 's -> Turtle.command list
}
let sys : symbol system =
{
axiom = Seq [Symb A ; ];
rules = 
		(function 
		| A -> Seq [Symb A ; Branch (Seq [Symb B ; Symb A ; ]) ; Symb A ; Branch (Seq [Symb C ; Symb A ; ]) ; Symb A ; ]
		| s -> Symb s
		);
interp = 
		(function 
		| A -> [Line (5)]
		| B -> [Turn (25)]
		| C -> [Turn (-25)]
		);
}
