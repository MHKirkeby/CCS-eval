
structure Main =
struct


	fun createLexerStream ( is : BasicIO.instream ) =
	    Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

	(* fun do_lex filename = createLexerStream (BasicIO.open_in filename);
	   fun do_parse lexbuf = (Parser.Ctrs Lexer.Token lexbuf);

	   val l1 = do_lex "1123.txt";
	   val p1 = do_parse l1;
	*)
	
	fun printctrs (filename, prg) =
	    let
		val outstream = TextIO.openAppend(filename)	
	    	val s = (Printctrs.ctrs prg)
    	    in
		(TextIO.output(outstream,s);
		 TextIO.closeOut(outstream))
            end;

	fun invert defs pend =
	    let val (ctype, vars, rules,comment) = defs
	    	val rules_inv = (Seminv.main_semiinv rules vars pend)
	    in
		(* val 'c main_semiinv :
  Rule list -> (string * 'c) list -> (string * int list * int list) list ->
  Rule list *)
	    	(ctype, vars, rules_inv, comment)
	    end;

	
	fun printpend (s,iset,oset) =
	    let val istring = String.concatWith "," (map Int.toString iset)
	    	val ostring = String.concatWith "," (map Int.toString oset)
	        in "("^s^","^istring^ostring^")"
		end;

	fun printpends pends = "["^(String.concatWith "," (map printpend pends))^"]";

	fun compile filename_in filename_out Pend_Str =
	    let val lexbuf = createLexerStream (BasicIO.open_in filename_in)
	    	val defs = (Parser.Ctrs Lexer.Token lexbuf)
		val lexbuf2 = (Lexing.createLexerString Pend_Str)
		val pend = (PendParser.Pend PendLexer.Token lexbuf2)

(*		val defs_inv = invert defs  [("add",[1,2],[])]  *)
		val defs_inv = (invert defs  pend)
	    in
		(print(" "^filename_in^" : "(*^(printpends pend)^" : "*)^filename_out);
		 
		 printctrs(filename_out,defs_inv))
		(* Interpreter.evalDefs defs *)
	    end

	fun errorMess s = TextIO.output (TextIO.stdErr,s ^ "\n");

	val _ = compile (List.nth(Mosml.argv (),1)) (List.nth(Mosml.argv (),2)) (List.nth(Mosml.argv (),3))
	      	handle Parsing.yyexit ob => errorMess "Parser-exit\n"
		     | Parsing.ParseError ob =>
		       	 let val Location.Loc (p1,p2) = Location.getCurrentLocation ()
			     val (lin,col) = Lexer.getLineCol p2 (!Lexer.currentLine) (!Lexer.lineStartPos)
			 in
			     errorMess ("Parse-error at line "^ makestring lin ^ ", column " ^ makestring col)
			 end
		     | Lexer.LexicalError (mess,(lin,col)) => errorMess ("Lexical error: " ^mess^ " at line "
^ makestring lin ^ ", column " ^ makestring col)
  	     	     | SysErr (s,_) => errorMess ("Exception: " ^ s)
end
