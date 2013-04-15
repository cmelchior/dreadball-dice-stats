mosmlyac -v Parser.grm
mosmllex Lexer.lex
mosmlc -c Syntax.sml
mosmlc -c Parser.sig
mosmlc -c Parser.sml
mosmlc -c Lexer.sml
mosmlc -c Interpreter.sig
mosmlc -c Interpreter.sml
mosmlc -c Distribution.sig
mosmlc -c Distribution.sml
mosmlc -o troll.exe Main.sml


