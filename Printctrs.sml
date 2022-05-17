
structure Printctrs =
struct


	fun ctype(Syntax.SEMIEQUATIONAL(_)) = "SEMI-EQUATIONAL"
	| ctype(Syntax.JOIN(_)) = "JOIN"
	| ctype(Syntax.ORIENTED(_)) = "ORIENTED";
 
	fun var [] = ""
	|   var ((vname,_)::vs) =  " "^vname^var(vs);

	fun termlist([]) = "nil"
	| termlist([t]) = term(t)
	| termlist(t::ts) = term(t)^", "^termlist(ts)
	
	and term(Syntax.ID(s,_))= s
	   | term( Syntax.FSYMB(s,[],_)) = s
	   | term( Syntax.FSYMB(s,t::ts,_)) = s^"("^termlist(t::ts)^")";


	(* for printing out list of substitutions (string * Term) list list *)
	fun sub [] = ""
	 | sub ((str, t)::ss) = "("^str^", "^(term t)^")"^(sub ss);
	 
	fun sublist [] = ""
	  | sublist (s::ss) = "["^(sub s)^"] "^(sublist ss)

	(* for printing out terms within Interpreter -uses term *)
	fun 
	termlist_newline([(t,no)]) = term(t)^", "^Int.toString(no)
	| termlist_newline((t,no)::ts) = term(t)^", "^Int.toString(no)^" \n"^termlist_newline(ts);
	
	fun cond(Syntax.COND(t1,t2,_)) = term(t1)^" == "^term(t2);

	fun condlist([t]) = cond(t)
	| condlist(t::ts) = cond(t)^","^condlist(ts);


	(* create nice indentations:  start *)
	fun getheaderlength(Syntax.CRULE(lhs,rhs,conds,_))  =
	    String.size(term(lhs)^" -> "^term(rhs));

	fun maxOf(s1, s2) = if s1>s2 then s1 else s2;

	fun prerun rules = let val sizes = map getheaderlength rules
	    in
		foldr maxOf 0 sizes
		end;

	fun addempty n = if n>0 then " "^addempty(n-1) else "";
	
	(* create nice indentations:  end *)
	fun rule(Syntax.CRULE(lhs,rhs,conds,_),m) =
	if conds=[] then "\t"^term(lhs)^" -> "^term(rhs)^"\n"
	   	    else
		    let val header = "\t"^term(lhs)^" -> "^term(rhs)
		    in
			header^addempty((m - String.size(header) + 1))^" | "^condlist(conds)^"\n"
		    end;
	
	fun rules ([],_) = ""
	| rules (r::rs,m) = rule(r,m)^rules(rs,m);

	fun prettyrules rs = let val maxSize = prerun rs in
	    rules (rs, maxSize) end;		
	

	fun ctrs(c, v, r, Syntax.COMMENT(comment,_)) =
	    let val s_c = ctype(c) 
	    	val s_v = var(v)
		val s_r = prettyrules(r)
	    in
	    "(CONDITIONTYPE " ^ s_c ^ ") \n" ^ "(VAR"^s_v^") \n"^"(RULES\n"^s_r^")\n"^comment ^ "\n"
	    end;
	    
fun print_to_file (filename, text) =
    let
	val outstream = TextIO.openAppend(filename)	
    in
    	(TextIO.output(outstream,text);
	TextIO.closeOut(outstream))
    end;

fun printctrs (filename, prg) =
    let
	val text = ctrs(prg)
    in
    	print_to_file(filename, text)
    end;

end

(* Test:
   printctrs("1123_new.txt",p1);

*)