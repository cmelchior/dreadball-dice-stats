local open Obj Lexing in



 open Lexing;
 val currentLine = ref 1
 val lineStartPos = ref [0]

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
                                (!currentLine)
                                (!lineStartPos)

 and getLineCol pos line (p1::ps) =
       if pos>=p1 then (line, pos-p1)
       else getLineCol pos (line-1) ps
   | getLineCol p l [] = (0,0) (* should not happen *)


 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 fun lexerError lexbuf s = 
     raise LexicalError (s, getPos lexbuf)

 fun keyword (s, pos) =
     case s of
         "D"            => Parser.D pos
       | "d"            => Parser.D pos
       | "Z"            => Parser.Z pos
       | "z"            => Parser.Z pos
       | "U"            => Parser.CONC pos
       | "sum"          => Parser.SUM pos
       | "least"        => Parser.LEAST pos
       | "largest"      => Parser.LARGEST pos
       | "count"        => Parser.COUNT pos
       | "drop"         => Parser.DROP pos
       | "keep"         => Parser.KEEP pos
       | "pick"         => Parser.PICK pos
       | "median"       => Parser.MEDIAN pos
       | "let"          => Parser.LET pos
       | "in"           => Parser.IN pos
       | "repeat"       => Parser.REPEAT pos
       | "accumulate"   => Parser.ACCUM pos
       | "while"        => Parser.WHILE pos
       | "until"        => Parser.UNTIL pos
       | "foreach"      => Parser.FOREACH pos
       | "do"           => Parser.DO pos
       | "if"           => Parser.IF pos
       | "then"         => Parser.THEN pos
       | "else"         => Parser.ELSE pos
       | "min"          => Parser.MIN pos
       | "max"          => Parser.MAX pos
       | "minimal"      => Parser.MINIMAL pos
       | "maximal"      => Parser.MAXIMAL pos
       | "choose"       => Parser.CHOOSE pos
       | "different"    => Parser.DIFFERENT pos
       | "function"     => Parser.FUNCTION pos
       | "call"         => Parser.CALL pos
       | "compositional"
	                => Parser.COMPOSITIONAL pos
       | _              => Parser.ID (s,pos);

 
fun action_39 lexbuf = (
 lexerError lexbuf "Illegal symbol in input" )
and action_38 lexbuf = (
 Parser.EOF (getPos lexbuf) )
and action_37 lexbuf = (
 Parser.SAMPLE (getPos lexbuf) )
and action_36 lexbuf = (
 Parser.VCONCC (getPos lexbuf) )
and action_35 lexbuf = (
 Parser.VCONCR (getPos lexbuf) )
and action_34 lexbuf = (
 Parser.VCONCL (getPos lexbuf) )
and action_33 lexbuf = (
 Parser.HCONC (getPos lexbuf) )
and action_32 lexbuf = (
 Parser.QUESTION (getPos lexbuf) )
and action_31 lexbuf = (
 Parser.HASH (getPos lexbuf) )
and action_30 lexbuf = (
 Parser.AND (getPos lexbuf) )
and action_29 lexbuf = (
 Parser.CONC (getPos lexbuf) )
and action_28 lexbuf = (
 Parser.DOTDOT (getPos lexbuf) )
and action_27 lexbuf = (
 Parser.GE (getPos lexbuf) )
and action_26 lexbuf = (
 Parser.LE (getPos lexbuf) )
and action_25 lexbuf = (
 Parser.GT (getPos lexbuf) )
and action_24 lexbuf = (
 Parser.LT (getPos lexbuf) )
and action_23 lexbuf = (
 Parser.NEQ (getPos lexbuf) )
and action_22 lexbuf = (
 Parser.EQ (getPos lexbuf) )
and action_21 lexbuf = (
 Parser.ASSGN (getPos lexbuf) )
and action_20 lexbuf = (
 Parser.RBRACE (getPos lexbuf) )
and action_19 lexbuf = (
 Parser.LBRACE (getPos lexbuf) )
and action_18 lexbuf = (
 Parser.SEMI (getPos lexbuf) )
and action_17 lexbuf = (
 Parser.COMMA (getPos lexbuf) )
and action_16 lexbuf = (
 Parser.RPAR (getPos lexbuf) )
and action_15 lexbuf = (
 Parser.LPAR (getPos lexbuf) )
and action_14 lexbuf = (
 Parser.DIVIDE (getPos lexbuf) )
and action_13 lexbuf = (
 Parser.TIMES (getPos lexbuf) )
and action_12 lexbuf = (
 Parser.SETMINUS (getPos lexbuf) )
and action_11 lexbuf = (
 Parser.MINUS (getPos lexbuf) )
and action_10 lexbuf = (
 Parser.PLUS (getPos lexbuf) )
and action_9 lexbuf = (
 keyword (getLexeme lexbuf, getPos lexbuf) )
and action_8 lexbuf = (
 Parser.STRINGS (StringToken lexbuf, getPos lexbuf) )
and action_7 lexbuf = (
 case Real.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad number"
                             | SOME p => Parser.REAL (p, getPos lexbuf)
                        )
and action_6 lexbuf = (
 case Int.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad integer"
                             | SOME i => Parser.NUM (i, getPos lexbuf)
                        )
and action_5 lexbuf = (
 Token lexbuf )
and action_4 lexbuf = (
 currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
                                           :: !lineStartPos;
                          Token lexbuf )
and action_3 lexbuf = (
 Token lexbuf )
and action_2 lexbuf = (
 getLexeme lexbuf :: StringToken lexbuf )
and action_1 lexbuf = (
 getLexeme lexbuf :: StringToken lexbuf )
and action_0 lexbuf = (
 [] )
and state_0 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_2);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\^@" andalso currChar <= #"\^_" then  backtrack lexbuf
 else if currChar >= #"\127" andalso currChar <= #"\159" then  backtrack lexbuf
 else case currChar of
    #"|" => state_51 lexbuf
 |  #"<" => state_50 lexbuf
 |  #"\"" => action_0 lexbuf
 |  _ => state_48 lexbuf
 end)
and state_1 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_27 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_27 lexbuf
 else if currChar >= #"1" andalso currChar <= #"9" then  state_19 lexbuf
 else case currChar of
    #"\t" => action_3 lexbuf
 |  #"\r" => action_3 lexbuf
 |  #" " => action_3 lexbuf
 |  #"\n" => action_4 lexbuf
 |  #"\f" => action_4 lexbuf
 |  #"}" => action_20 lexbuf
 |  #"|" => state_30 lexbuf
 |  #"{" => action_19 lexbuf
 |  #"\\" => state_28 lexbuf
 |  #"@" => action_29 lexbuf
 |  #"?" => action_32 lexbuf
 |  #">" => state_24 lexbuf
 |  #"=" => state_23 lexbuf
 |  #"<" => state_22 lexbuf
 |  #";" => action_18 lexbuf
 |  #":" => state_20 lexbuf
 |  #"0" => state_18 lexbuf
 |  #"/" => action_14 lexbuf
 |  #"." => state_16 lexbuf
 |  #"-" => state_15 lexbuf
 |  #"," => action_17 lexbuf
 |  #"+" => action_10 lexbuf
 |  #"*" => action_13 lexbuf
 |  #")" => action_16 lexbuf
 |  #"(" => action_15 lexbuf
 |  #"'" => action_37 lexbuf
 |  #"&" => action_30 lexbuf
 |  #"#" => action_31 lexbuf
 |  #"\"" => action_8 lexbuf
 |  #"\^@" => action_38 lexbuf
 |  _ => action_39 lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_11);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => action_12 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_39);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"." => action_28 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_18 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_43 lexbuf
 else case currChar of
    #"." => state_44 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_19 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_43 lexbuf
 else backtrack lexbuf
 end)
and state_20 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_39);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_21 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_22 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_24);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"|" => action_35 lexbuf
 |  #">" => action_36 lexbuf
 |  #"=" => action_26 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_23 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_22);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => state_37 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_24 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_27 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_27 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_9);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_35 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_35 lexbuf
 else backtrack lexbuf
 end)
and state_28 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  #"\f" => backtrack lexbuf
 |  _ => state_34 lexbuf
 end)
and state_30 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_39);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"|" => action_33 lexbuf
 |  #">" => action_34 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_34 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  #"\f" => backtrack lexbuf
 |  _ => state_34 lexbuf
 end)
and state_35 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_9);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_35 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_35 lexbuf
 else backtrack lexbuf
 end)
and state_37 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_23 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_43 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_43 lexbuf
 else backtrack lexbuf
 end)
and state_44 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_45 lexbuf
 else backtrack lexbuf
 end)
and state_45 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_45 lexbuf
 else backtrack lexbuf
 end)
and state_48 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_2);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\^@" andalso currChar <= #"\^_" then  backtrack lexbuf
 else if currChar >= #"\127" andalso currChar <= #"\159" then  backtrack lexbuf
 else case currChar of
    #"\"" => backtrack lexbuf
 |  #"|" => state_54 lexbuf
 |  #"<" => state_53 lexbuf
 |  _ => state_48 lexbuf
 end)
and state_50 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\^@" andalso currChar <= #"\^_" then  backtrack lexbuf
 else if currChar >= #"\127" andalso currChar <= #"\159" then  backtrack lexbuf
 else case currChar of
    #"\"" => backtrack lexbuf
 |  #">" => action_1 lexbuf
 |  #"|" => action_1 lexbuf
 |  _ => state_48 lexbuf
 end)
and state_51 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\^@" andalso currChar <= #"\^_" then  backtrack lexbuf
 else if currChar >= #"\127" andalso currChar <= #"\159" then  backtrack lexbuf
 else case currChar of
    #"\"" => backtrack lexbuf
 |  #">" => action_1 lexbuf
 |  #"|" => action_1 lexbuf
 |  _ => state_48 lexbuf
 end)
and state_53 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\^@" andalso currChar <= #"\^_" then  backtrack lexbuf
 else if currChar >= #"\127" andalso currChar <= #"\159" then  backtrack lexbuf
 else case currChar of
    #"\"" => backtrack lexbuf
 |  #">" => backtrack lexbuf
 |  #"|" => backtrack lexbuf
 |  _ => state_48 lexbuf
 end)
and state_54 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\^@" andalso currChar <= #"\^_" then  backtrack lexbuf
 else if currChar >= #"\127" andalso currChar <= #"\159" then  backtrack lexbuf
 else case currChar of
    #"\"" => backtrack lexbuf
 |  #">" => backtrack lexbuf
 |  #"|" => backtrack lexbuf
 |  _ => state_48 lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_1 lexbuf)

and StringToken lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_39, action_38, action_37, action_36, action_35, action_34, action_33, action_32, action_31, action_30, action_29, action_28, action_27, action_26, action_25, action_24, action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3];
val _ = fn _ => [action_2, action_1, action_0];

end
