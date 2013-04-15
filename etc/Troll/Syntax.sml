structure Syntax =
struct

  type pos = int * int       (* position in program (line,column) *)

  datatype Exp = NUM of int * pos 
	       | ID of string * pos  
	       | EMPTY
	       | CONC of Exp * Exp * pos 
	       | FROMTO of Exp * Exp * pos
	       | CHOOSE of Exp * pos 
	       | DIFFERENT of Exp * pos 
	       | PLUS of Exp * Exp * pos   
	       | MINUS of Exp * Exp * pos   
	       | TIMES of Exp * Exp * pos   
	       | DIVIDE of Exp * Exp * pos    
	       | UMINUS of Exp * pos
	       | D of Exp * pos
	       | Z of Exp * pos 
	       | SUM of Exp * pos
	       | COUNT of Exp * pos
	       | LEAST of Exp * Exp * pos
	       | LARGEST of Exp * Exp * pos
	       | MINIMAL of Exp * pos
	       | MAXIMAL of Exp * pos
	       | HASH of Exp * Exp * pos
	       | AND of Exp * Exp * pos
	       | EQ of Exp * Exp * pos
	       | NEQ of Exp * Exp * pos
	       | LT of Exp * Exp * pos
	       | GT of Exp * Exp * pos
	       | LE of Exp * Exp * pos
	       | GE of Exp * Exp * pos
	       | DROP of Exp * Exp * pos
	       | KEEP of Exp * Exp * pos
	       | PICK of Exp * Exp * pos
	       | SETMINUS of Exp * Exp * pos
	       | MEDIAN of Exp * pos
	       | LET of string * Exp * Exp * pos
	       | REPEAT of string * Exp * Exp * bool * pos 
               | ACCUM of string * Exp * Exp * bool * pos
	       | FOREACH of string * Exp * Exp * pos
	       | IF of Exp * Exp * Exp * pos
	       | CALL of string * Exp list * pos
	       | STRING of string * pos
               | SAMPLE of Exp * pos
               | SAMPLES of Exp * Exp * pos
               | HCONC of Exp * Exp * pos
               | VCONCL of Exp * Exp * pos
               | VCONCR of Exp * Exp * pos
               | VCONCC of Exp * Exp * pos
	       | QUESTION of real * pos


  (* (name, (args, body, position)) *)
  datatype Declaration = Func of (string list * Exp * pos)
                       | Comp of (Exp * string * string * pos)

  type Program = (string * Declaration) list * Exp

end
