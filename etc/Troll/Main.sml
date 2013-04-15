structure Main =
struct

  exception ParamErr of string

  val percent = ref true

  val graph = ref 0.0

  fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

  fun print s = TextIO.output (TextIO.stdOut, s)

  fun stringVal l =
        String.concat
	  (List.map
	    (fn n => if n>=0 then Int.toString n ^ " "
	             else "-" ^ Int.toString (~n) ^ " ")
	    l)

  fun realtostring n = if n<0.0 then "-" ^ Real.toString (~n)
		       else Real.toString n

  fun padrealtostring p =
	let
	  val ps = if p<9.999999
		   then "  "^realtostring p
		   else if p<100.0
		   then " "^realtostring p
		   else realtostring p
	in
	  ps ^ String.substring("                 ",
			        0,20-String.size ps)
	end

  fun stringIVal (Interpreter.VAL l) = stringVal l
    | stringIVal (Interpreter.TEXT ts) =
        String.concat (List.map (fn t=>t ^"\n") ts)

  fun printVal v = print (stringIVal v ^"\n")

  fun gtList [] _ = false
    | gtList (a::l1) [] = true
    | gtList (a::l1) (b::l2) = a>b orelse a=b andalso gtList l1 l2


  fun printDist1 [] pad = ()
    | printDist1 ((a,p)::l) pad =
        let
          val s1 = stringIVal a
	  val s2 = case a of
		     Interpreter.VAL _ =>
		       String.substring(pad,
					0,String.size pad - String.size s1)
		   | Interpreter.TEXT _ => ""
	  val s2' = case a of
		     Interpreter.VAL _ => ""
		   | Interpreter.TEXT _ => pad
	  val s3 = padrealtostring p
	  val pgeq = p + List.foldr (op +) 0.0 (List.map (#2) l)
	  val s4 = padrealtostring pgeq
          fun bar p = if (!graph)*p<0.5 then []
                      else #"|" :: bar (p-1.0/(!graph))
	in
          (if !graph=0.0
           then print (s2 ^ s1 ^ s2' ^ ":  " ^ s3 ^ s4 ^ "\n")
           else  print (s2 ^ s1 ^ s2' ^ ":  " ^ implode (bar p) ^ "\n");
	   printDist1 l pad)
	end

  fun printDist l =
        let
	  fun myAdd ((Interpreter.VAL (n::_),p),s) = p*(Real.fromInt n)+s
	    | myAdd (_,s) = s
	  fun myAdd2 ((Interpreter.VAL (n::_),p),s) =
	        p*(Real.fromInt n)*(Real.fromInt n)+s
	    | myAdd2 (_,s) = s
	  fun myAdd3 m ((Interpreter.VAL (n::_),p),s) =
	        p*(Real.abs (Real.fromInt n - m))+s
	    | myAdd3 m (_,s) = s
	  val maxLen = List.foldr Int.max 0
		        (List.map (fn (Interpreter.VAL a,_) =>
				      String.size (stringVal a)
				    | (Interpreter.TEXT ts,_) =>
				        List.foldr Int.max 0
						   (List.map String.size ts))
				  l)
	  val pad = String.implode (List.tabulate (maxLen+5, fn x => #" "))
	  val s1 = "Value"
	  val s2 = String.substring(pad,
				    0,String.size pad - String.size s1)
	  val s3 = if !percent
	           then "    % =              "
		   else "  Probability for =  "
	  val s4 = if !percent
	           then "   % >=              "
		   else " Probability for >=  "
	  val mean = if List.all (fn (Interpreter.VAL x,p)
				     => List.length x<=1
				   | _ => false)
				 l
		     then
                       let
                         val m = List.foldr myAdd 0.0 l
                         val m2 = List.foldr myAdd2 0.0 l
			 val md = List.foldr (myAdd3 m) 0.0 l
                       in
		         SOME (m, Math.sqrt(m2-m*m), md)
		       end
		     else NONE
	  val s5 = case mean of
		     NONE => ""
		   | SOME (m,sp,md) =>
		       "\nAverage = " ^ realtostring m ^
		       "    Spread = " ^ realtostring sp ^
		       "  Mean deviation = " ^ realtostring md ^ "\n"
	  val l1 = if !percent
	           then List.map (fn (a,p)=>(a,100.0*p)) l
		   else l
        in
	  (if !graph=0.0
           then print (s2 ^ s1 ^ s3 ^ s4 ^ "\n")
           else print (s2 ^ s1 ^"   "^ Real.toString (!graph) ^" bars per %\n");
	   printDist1 l1 pad;
	   print s5)
	end

  fun run filename n defs =
        let
          val lexbuf =  createLexerStream
			  (if filename = "" then BasicIO.std_in
		           else BasicIO.open_in filename)
          val dice = 
            let
              val (decls,exp) = Parser.Dice Lexer.Token lexbuf
            in
              (decls, defs exp)
            end
        in
          (let
	     prim_val getrealtime_ : unit -> {sec : int, usec : int} 
                                = 1 "sml_getrealtime"
             val {sec, usec} = getrealtime_ ()
	   in
	     (Interpreter.seed := sec mod 100001 + 5*(usec mod 77771);
	      Interpreter.seed2 := sec mod 3557 + 7*(usec mod 3307))
	   end;
	   if n<0 then (Distribution.maxiterations := ~n;
		        printDist (Distribution.distribDice dice))
	   else if n=0 then printDist (Distribution.distribDice dice)
	   else
	     hd (List.tabulate
	         (n,fn x => printVal (Interpreter.rollDice dice)))
	  )
        end

  fun errorMess s = TextIO.output (TextIO.stdErr,s ^ "\n")

  fun findDef str =
        case String.fields (fn c=>c = #"=") str of
          [name,valString] =>
            (case Int.fromString valString of
	       NONE => NONE
	     | SOME value => SOME (name,value))
        | _ => NONE

  fun run0 [] filename n defs = run filename n defs
    | run0 (arg::args) filename n defs =
	case Int.fromString arg of
	  NONE => (case findDef arg of
		     NONE =>
		       if arg = "-p"
		       then (percent := not (!percent);
			     run0 args filename n defs)
		       else if "-g" <= arg andalso arg < "-h"
		       then (case Real.fromString
                                    (String.extract (arg,2,NONE)) of
                               NONE => raise ParamErr arg
                             | SOME s =>
                               (graph := s;
			        run0 args filename n defs))
		       else run0 args arg n defs
		   | SOME (name,value) =>
		       run0 args filename n
			    (fn d=>Syntax.LET
                                     (name, Syntax.NUM (value,(0,0)),
				      defs d,(0,0))))
	| SOME n => run0 args filename n defs

  val _ =
    let
      val argv = Mosml.argv ()
    in
      run0 (tl argv) "" 1 (fn d=>d)
    end
    handle Parsing.yyexit ob => errorMess "Parser-exit\n"
         | Parsing.ParseError ob =>
             let val Location.Loc (p1,p2)
                       = Location.getCurrentLocation ()
                 val (lin,col)
                        = Lexer.getLineCol p2
                                          (!Lexer.currentLine)
                                          (!Lexer.lineStartPos)

             in
               errorMess ("Parse-error at line "
                ^ makestring lin ^ ", column " ^ makestring col)
             end
         | Lexer.LexicalError (mess,(lin,col)) =>
               errorMess ("Lexical error: " ^mess^ " at line "
                ^ makestring lin ^ ", column " ^ makestring col)
	 | Interpreter.RunError (mess,(lin,col)) =>
               errorMess ("Runtime error: " ^mess^ " at line "
                ^ makestring lin ^ ", column " ^ makestring col)
	 | Distribution.DistribError (mess,(lin,col)) =>
               errorMess ("Distribution error: " ^mess^ " at line "
                ^ makestring lin ^ ", column " ^ makestring col)
         | SysErr (s,_) => errorMess ("Exception: " ^ s)
         | ParamErr s => errorMess ("Bad command-line parameter: " ^ s)
end

