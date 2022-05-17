(*
structure MainEval =
struct
*)

	fun createLexerStream ( is : BasicIO.instream ) =
	    Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

	(* fun do_lex filename = createLexerStream (BasicIO.open_in filename);
	   fun do_parse lexbuf = (Parser.Ctrs Lexer.Token lexbuf);

	   val l1 = do_lex "1123.txt";
	   val p1 = do_parse l1;
	*)
	
	fun printterms (filename, input_term, terms, steps, calls) =
	    let
		val outstream = TextIO.openAppend(filename)	
			val s_steps = (Int.toString(steps)) 
			val s_calls = (Int.toString(calls)) 
	    	val s_terms = (Printctrs.termlist terms)
	    	val s_term = (Printctrs.term input_term)
    	    in
		(   TextIO.output(outstream,("steps: "^s_steps^" \t calls: "^s_calls^"\t ")); 
			TextIO.output(outstream,("input: "^s_term^" \t output terms: "^s_terms^" \n"));
		 TextIO.closeOut(outstream))
            end;

    fun clean (a,_) = a; 

	fun eval defs term =
	    let val (ctype, vars, rules,comment) = defs
	    	val clean_vars = (map clean vars)
	    	(* val (evals, steps) = (Interpreter.main_eval rules vars term) *)
	    in
		(* val 'c main_semiinv :
  Rule list -> (string * 'c) list -> (string * int list * int list) list ->
  Rule list *)
  
  			(* main_eval term systemrules systemvars *)
  			(Interpreter.main_eval term rules clean_vars)
	    end;

	
	fun printpend (s,iset,oset) =
	    let val istring = String.concatWith "," (map Int.toString iset)
	    	val ostring = String.concatWith "," (map Int.toString oset)
	        in "("^s^","^istring^ostring^")"
		end;

	fun printpends pends = "["^(String.concatWith "," (map printpend pends))^"]";

	fun compile filename_in filename_out Term_Str =
	    let val lexbuf = createLexerStream (BasicIO.open_in filename_in)
	    	val defs = (Parser.Ctrs Lexer.Token lexbuf)
			val lexbuf2 = (Lexing.createLexerString Term_Str)
			val term = (Parser.Term Lexer.Token lexbuf2)
(*		val defs_inv = invert defs  [("add",[1,2],[])]  *)
			val (output_terms, steps,calls) = (eval defs term)
	    in
		 
		 printterms(filename_out, term, output_terms, steps, calls)
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
(*
end *)