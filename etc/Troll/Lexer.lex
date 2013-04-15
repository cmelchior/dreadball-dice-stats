{

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

 }

rule Token = parse
    [` ` `\t` `\r`]     { Token lexbuf } (* whitespace *)
  | [`\n` `\012`]       { currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
                                           :: !lineStartPos;
                          Token lexbuf } (* newlines *)
  | "\\" [^ `\n` `\012`]*
                        { Token lexbuf } (* comment *)
  | [`0`-`9`]+          { case Int.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad integer"
                             | SOME i => Parser.NUM (i, getPos lexbuf)
                        }
  | "0."[`0`-`9`]+ 	{ case Real.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad number"
                             | SOME p => Parser.REAL (p, getPos lexbuf)
                        }
  | `"`                 { Parser.STRINGS (StringToken lexbuf, getPos lexbuf) }
  | [`a`-`z` `A`-`Z`]+  { keyword (getLexeme lexbuf, getPos lexbuf) }
  | `+`                 { Parser.PLUS (getPos lexbuf) }
  | `-`                 { Parser.MINUS (getPos lexbuf) }
  | "--"                { Parser.SETMINUS (getPos lexbuf) }
  | `*`                 { Parser.TIMES (getPos lexbuf) }
  | `/`                 { Parser.DIVIDE (getPos lexbuf) }
  | `(`                 { Parser.LPAR (getPos lexbuf) }
  | `)`                 { Parser.RPAR (getPos lexbuf) }
  | `,`                 { Parser.COMMA (getPos lexbuf) }
  | `;`                 { Parser.SEMI (getPos lexbuf) }
  | `{`                 { Parser.LBRACE (getPos lexbuf) }
  | `}`                 { Parser.RBRACE (getPos lexbuf) }
  | ":="                { Parser.ASSGN (getPos lexbuf) }
  | `=`                 { Parser.EQ (getPos lexbuf) }
  | "=/="               { Parser.NEQ (getPos lexbuf) }
  | `<`                 { Parser.LT (getPos lexbuf) }
  | `>`                 { Parser.GT (getPos lexbuf) }
  | "<="                { Parser.LE (getPos lexbuf) }
  | ">="                { Parser.GE (getPos lexbuf) }
  | ".."                { Parser.DOTDOT (getPos lexbuf) }
  | `@`                 { Parser.CONC (getPos lexbuf) }
  | `&`                 { Parser.AND (getPos lexbuf) }
  | `#`                 { Parser.HASH (getPos lexbuf) }
  | `?`                 { Parser.QUESTION (getPos lexbuf) }
  | "||"                { Parser.HCONC (getPos lexbuf) }
  | "|>"                { Parser.VCONCL (getPos lexbuf) }
  | "<|"                { Parser.VCONCR (getPos lexbuf) }
  | "<>"                { Parser.VCONCC (getPos lexbuf) }
  | `'`                 { Parser.SAMPLE (getPos lexbuf) }
  | eof                 { Parser.EOF (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

and  StringToken = parse
    `"`                 { [] }
  | "|>" | "<|" | "<>" | "||"
                        { getLexeme lexbuf :: StringToken lexbuf }
  | (  [^ `\000`-`\031` `"` `|` `<` `\127`-`\159`]
     | `|` [^ `\000`-`\031` `"` `|` `>` `\127`-`\159`]
     | `<` [^ `\000`-`\031` `"` `|` `>` `\127`-`\159`]) *
                        { getLexeme lexbuf :: StringToken lexbuf }
;
