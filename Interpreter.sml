(************************************************************************)
(* The following CTRS evaluator is a DFS evaluator of CTRS evaluating left-to-right.  *) 

(* TO DO 
1. udkommenter alt undtagen apply_subst og start med at checke programmet
2. skriv compose
3. skriv match og du er nød til at sikre dig at rækkefølgen passer ved alle kald til den.
4. check resten af programmet
*)


structure Interpreter =
struct

(* 
load "Syntax";
*)

val allvars = ref []
val rules = ref []
val indent = ref 0;

val step_count = ref 0;
val call_count = ref 0;

fun i2t i = if i <= 0 then "" else "\t"^(i2t (i-1));
fun indent_str () = (i2t (!indent));
fun incr_indent () = indent := (!indent)+1;
fun decr_indent () = indent := (!indent)-1;

fun update_step () = let val step = (!step_count)
				  in
				  	 step_count := (step + 1)
				  end;
				  

fun update_call () = let val call = (!call_count)
				  in
				  	 call_count := (call + 1)
				  end;

				  
(* *)
fun is_member x [] = false
 | is_member x (y::ys) = if x=y then true else (is_member x ys);
 
 

(* apply_subst: term * subst -> subst *)

(*  datatype Term = ID of Id | FSYMB of string * Term list * pos    *)
(*  subst  = [(Syntax.ID, Syntax.Term )] 
	e.g., subst =  [ (Syntax.ID("x", _), Syntax.ID("0", _)),
					 (Syntax.ID("y", _), Syntax.FSYMB("s", [Syntax.ID("0",_)], _), _)]  
*)

(* subst = [(name, substitution_term)] *)
fun lookup name [] = NONE
  | lookup name ((name2, substitution_term)::subst) = 
 	if name=name2 then (SOME substitution_term) else (lookup name subst);

fun apply_subst subst (Syntax.FSYMB(name,terms,pos)) = 
	let val terms_update = (map (apply_subst subst) terms) 
	in (Syntax.FSYMB(name,terms_update,pos))
	end
 | apply_subst subst (Syntax.ID(name,pos))  = 
	 (case (lookup name subst) of
	 	  NONE => Syntax.ID(name,pos)
	 	| SOME value => value );
 
fun eq x y = (x=y) ;

(* match: expr * term (e.g., rule-head)  ->  										*)
(* 	   NONE																			*)
(* 	 | SOME subst																	*)
(* match term1 term2 
  term1 = subst(term2) ?
*)

(* is true if no occurrences of varname in term *)
fun no_occurrence_check varname term = 
	let 
		val const = varname^"test"
		val term1 = (apply_subst [(varname ,Syntax.ID(const, (0, 0)))] term)
	in
		term = term1
	end;

fun equal_term (Syntax.FSYMB(name1,terms1,_)) (Syntax.FSYMB(name2,terms2,_)) = (name1 = name2 andalso (eq_terms terms1 terms2))
| equal_term (Syntax.ID(name1,_)) (Syntax.ID(name2,_)) = (name1 = name2)
| equal_term x y = false

and eq_terms [] [] = true
  | eq_terms (t1::ts1) (t2::ts2) = (equal_term t1 t2) andalso (eq_terms ts1 ts2);


fun add_sub sub subs = let val (name,term) = sub 
							(*
							val str_term = "is "^(Printctrs.sub([sub]))^" in "^(Printctrs.sub(subs))^": "
							val _ = (print str_term) *)
					   in 
					   	  case (lookup name subs) of 
					        NONE => 
					        	(SOME (sub::subs))
					        	
						 | SOME term2 => 
						 		if (equal_term term term2)
						 		then (SOME subs)
						 		else NONE
					   end;
						 
fun match term1 (Syntax.FSYMB(name2,terms2,pos2)) sub = 
	(case term1 of 
		  Syntax.FSYMB(name1,terms1,pos1) => 
				if name1=name2 andalso (length terms1)=(length terms2) 
				then (match_list terms1 terms2 sub) 
				else NONE
		| Syntax.ID(_,_) => NONE)
		
 |match term1 (Syntax.ID(name2,pos2)) sub =
		
		if (List.exists (eq name2) (!allvars)) 
		then 
			(* name2 is a variable *)
			(case term1 of 
			  Syntax.FSYMB(name1,terms1,pos1) => 
			  	(* occurrence check *)
			  	if (no_occurrence_check name1 (Syntax.FSYMB(name1,terms1,pos1)))
			  	then (add_sub (name2,term1) sub)
			  	else NONE 
			| Syntax.ID(name1,pos1) => 
				(* occurrence check *)
				if not(name1 = name2) 
				then (add_sub (name2,Syntax.ID(name1,(0,0))) sub)
			  	else SOME sub)  
		else  
		    (* name2 is a constant *)
		 	(case term1 of 
		  	  Syntax.FSYMB(name1,terms1,pos1) => NONE
			| Syntax.ID(name1,pos1) 		  => if name1=name2 then (SOME sub) else NONE )
			
and match_list [] [] sub = SOME sub
  | match_list (term1::terms1) (term2::terms2) sub = 
 		(case (match term1 term2 sub) of
 		   SOME sub_upd => (match_list terms1 terms2 sub_upd)
 		 | NONE => NONE);

	

		
(* compose: subst_old * sub_new -> subst  *)
(* subst_old = { (x, term1), (y,term2)}  
sub_new = {(y, term3), (z,term4)}

subst = {(x, sub_new(term1)), (y,sub_new(term2)), (z,term4)}

according to, e.g., http://www.mathcs.duq.edu/simon/Fall04/notes-7-4/node4.html
*)



fun apply_subst_to_terms [] sub_new = []
 |  apply_subst_to_terms ((x, term_val)::sub_old) sub_new = 
 	 (x, (apply_subst sub_new term_val)) :: (apply_subst_to_terms sub_old sub_new);

fun add_new_subst [] sub_old = sub_old
 | add_new_subst ((x, term_val)::sub_new) sub_old = 
	case (lookup x sub_old) of
		NONE => (x, term_val)::(add_new_subst sub_new sub_old)
	  | SOME _ => (add_new_subst sub_new sub_old);

fun compose sub_old sub_new = (add_new_subst sub_new  (apply_subst_to_terms sub_old sub_new));


(* (get_all_rhs rhs all_possible_conds_substitutions )  *)
fun get_all_rhs  rhs [] = [] 
|    get_all_rhs  rhs (subst2::rest) = ((apply_subst subst2 rhs) :: (get_all_rhs  rhs rest))



(* rewrite_cond: 																	*)
(* 		(l,r) * subst * rules-left-to-try -> 										*)
(*        NONE																		*)
(*      | SOME subst(l) * subst * (subst'',trace) * alternatives-rules-left-to-try 	*)

 
fun check_match r _ [] = []
  | check_match r sub_org (r_comp::possible_rewritings) =  
	case (match  r_comp r []) of
		NONE => (check_match r sub_org possible_rewritings)
 	  | SOME subst_cond => (compose sub_org subst_cond)::(check_match r sub_org possible_rewritings);
 
 	   
fun rewrite_cond (Syntax.COND(l,r,_)) subst =
 	let 
 		val l_subst = (apply_subst subst l)
 		val possible_rewritings = (eval_term l_subst) (* this returns a list of terms *)
 		val r_subst = (apply_subst subst r) (* apply org subst to right-hand side *)
 	in	
 		(check_match r_subst subst possible_rewritings) 		(* check that the rhs of a cond matches the *)
 	end   
 	

 		

and 

    rewrite_conds [] subst = [(subst)]
  | rewrite_conds (c::cs) subst = 
	let val substs = (rewrite_cond c subst) 
	in
		(*  for each of the substitutions from the first condition, apply rewrite_conds cs *)
		(rewrite_for_each cs substs)
	end
and

    rewrite_for_each cs [] = []
  | rewrite_for_each cs (s::ss) = ((rewrite_conds cs s)@(rewrite_for_each cs ss))
 

(* apply_rule: term * rule(l,r,cs) -> 												*)
(* 			  NONE 																	*)
(* 			| SOME term *  subst' * subst'(r) * trace * cs-alternatives				*)
	

and
 apply_rule term (Syntax.CRULE(lhs,rhs,conds,_)) =
 	(case (match term lhs []) of
 		NONE => let val _ = () (* (print ("tried to match "^Printctrs.term(term)^" to "^Printctrs.term(lhs)^" and failed\n")) *)
 				in
 				  []
 				end
 	  | SOME subst => let 
 	  					  val _ = incr_indent ()
 	  					  val string_rule = Printctrs.prettyrules([(Syntax.CRULE(lhs,rhs,conds,(0,0)))])
 	 					  val string_term = Printctrs.term(term)
 	 					  val _  = (print ((indent_str ())^"try   "^string_term^" using "^string_rule^"\n\n"))
 	 					  val _ = update_call () (*  increment the number of terms required to be rewritten *)
 	 					  val all_possible_conds = (rewrite_conds conds subst)
 	  				      val rhs_subst = (apply_subst subst rhs)  	  				  	  	
 	  				   in 
 	  				   	 
 	  				   	 let val rhs_solutions = (get_all_rhs rhs_subst all_possible_conds) (* returns a list of possible initializations of rhs_subst  *)
 	  				   	 	 val string_sol = Printctrs.sublist(all_possible_conds) 
 	  				   	 	 val string_sol_sub = Printctrs.termlist(rhs_solutions)
 	  				   	 	 val _  = (print ((indent_str ())^("tried "^string_term^" using "^string_rule^"\n")))
 	  				   	 	 val _  = (print ((indent_str ())^(string_sol^" solutions: "^string_sol_sub^"\n\n")))
 	  				   	 	 val _ = decr_indent ()
 	  					 in
 	  					 	if rhs_solutions = [] then rhs_solutions else (let val _ = update_step() in rhs_solutions end) 	  					 	
 	  					 end	
 	  					 
 	  				   end)

 		
(* rewrite: term rules -> [(terms,subst,trace)]					*)

and rewrite_all term [] = [] 
  | rewrite_all term (rule::rules) = 
  	let 
  		val rewritings = (apply_rule term rule)		
  	in
  			rewritings @ (rewrite_all term rules)
  	end
  	
(* rewrite: term -> [(terms,subst,trace)] *)


and eval_term term = (rewrite_all term (!rules));

fun main_eval term systemrules systemvars = 
    (allvars := systemvars; rules := systemrules; step_count := 0;
    let val terms = (eval_term term)
    	val steps = (!step_count)
    	val calls = (!call_count)
    in
    	(terms,steps,calls)
    end	
   )

 
end


  
(*  Invoking the interpreter
	val term = Syntax.FSYMB("add", [Syntax.FSYMB("s", [Syntax.ID("0", (0, 0))], (0, 0)), Syntax.FSYMB("s", [Syntax.ID("0", (0, 0))], (0, 0))], (0, 0));

    

    val rule1 = Syntax.CRULE(Syntax.FSYMB("add",
                                [Syntax.ID("0", (16, 7)),
                                 Syntax.ID("y", (16, 9))], (16, 3)),
                   Syntax.FSYMB("o", [Syntax.ID("y", (16, 17))], (16, 15)), [],
                   (16, 12));
   val rule2 = Syntax.CRULE(Syntax.FSYMB("add",
                                [Syntax.FSYMB("s", [Syntax.ID("x", (17, 9))],
                                              (17, 7)),
                                 Syntax.ID("y", (17, 12))], (17, 3)),
                   Syntax.FSYMB("o",
                                [Syntax.FSYMB("s", [Syntax.ID("z", (17, 22))],
                                              (17, 20))], (17, 18)),
                   [Syntax.COND(Syntax.FSYMB("add",
                                             [Syntax.ID("x", (17, 32)),
                                              Syntax.ID("y", (17, 34))],
                                             (17, 28)),
                                Syntax.FSYMB("o", [Syntax.ID("z", (17, 42))],
                                             (17, 40)), (17, 37))], (17, 15));

	(main_eval term [rule1,rule2] ["x","y","z"])
    
    
    val term2 = Syntax.FSYMB("upto", [Syntax.FSYMB("s", [Syntax.ID("0", (0, 0))], (0, 0))], (0, 0));

    

    val rule_upto1 = Syntax.CRULE(Syntax.FSYMB("upto",
                                [Syntax.ID("x", (16, 7))], (16, 3)),
                   Syntax.FSYMB("o", [Syntax.ID("0", (16, 17))], (16, 15)), [],
                   (16, 12));
    val rule_upto2 = Syntax.CRULE(Syntax.FSYMB("upto",
                                [Syntax.FSYMB("s", [Syntax.ID("x", (17, 9))],
                                              (17, 7))], (17, 3)),
                   Syntax.FSYMB("o",
                                [Syntax.FSYMB("s", [Syntax.ID("y", (17, 22))],
                                              (17, 20))], (17, 18)),
                   [Syntax.COND(Syntax.FSYMB("upto",
                                             [Syntax.ID("x", (17, 32))],
                                             (17, 28)),
                                Syntax.FSYMB("o", [Syntax.ID("y", (17, 42))],
                                             (17, 40)), (17, 37))], (17, 15));

	(main_eval term2 [rule_upto1,rule_upto2] ["x","y","z"])
  
    
    
    
    
    (Seminv.rules := [rule1,rule2]; Seminv.allvars:= ["x","y","z"]; Seminv.seminv [("add",[1,2],[])])
*)  