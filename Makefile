
eval:
	mosmlc -c Syntax.sml
	mosmlyac -v Parser.grm	
	mosmlc -c Parser.sig
	mosmlc -c Parser.sml
	mosmllex Lexer.lex
	mosmlc -c Lexer.sml
	echo " -------------------------------------------------- "
	mosmlc -c Pend.sml
	mosmlyac -v PendParser.grm	
	mosmlc -c PendParser.sig
	mosmlc -c PendParser.sml
	mosmllex PendLexer.lex
	mosmlc -c PendLexer.sml
	echo " -------------------------------------------------- "
	mosmlc -c Printctrs.sml
	mosmlc -c Interpreter.sml
	mosmlc -o MainEval MainEval.sml

all:
	mosmlc -c Syntax.sml
	mosmlyac -v Parser.grm	
	mosmlc -c Parser.sig
	mosmlc -c Parser.sml
	mosmllex Lexer.lex
	mosmlc -c Lexer.sml
	echo " -------------------------------------------------- "
	mosmlc -c Pend.sml
	mosmlyac -v PendParser.grm	
	mosmlc -c PendParser.sig
	mosmlc -c PendParser.sml
	mosmllex PendLexer.lex
	mosmlc -c PendLexer.sml
	echo " -------------------------------------------------- "
	mosmlc -c Printctrs.sml
	mosmlc -c Seminv.sml
	mosmlc -c Interpreter.sml
	mosmlc -o MainEval MainEval.sml
	mosmlc -o Main Main.sml

clean:
	rm MainEval.ui MainEval.uo Main.ui Main.uo Pend.ui Pend.uo Interpreter.uo Parser.output Parser.sig Parser.sml Parser.uo Parser.ui Syntax.ui Syntax.uo Lexer.sml Lexer.ui Lexer.uo PendLexer.sml PendLexer.ui PendLexer.uo PendParser.output PendParser.sig PendParser.sml PendParser.ui PendParser.uo Printctrs.uo Printctrs.ui Seminv.ui Seminv.uo

save:  
	zip save.zip Syntax.sml Parser.grm Lexer.lex Main Main.sml Printctrs.sml MainEval.sml Interpreter.sml Seminv.sml Makefile 
