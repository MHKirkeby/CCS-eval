(************************************************************************)
(* The following CTRS evaluator is a DFS evaluator of CTRS evaluating left-to-right.  *) 

(* TO DO 
1. udkommenter alt undtagen apply_subst og start med at checke programmet
2. skriv compose
3. skriv match og du er nød til at sikre dig at rækkefølgen passer ved alle kald til den.
4. check resten af programmet
*)

val allvars = ref []
val setrules = ref []


(* apply_subst: term * subst -> subst *)

(*  datatype Term = ID of Id | FSYMB of string * Term list * pos    *)
(*  subst  = [(Syntax.ID, Syntax.Term )] 
	e.g., subst =  [ (Syntax.ID("x", _), Syntax.ID("0", _)),
					 (Syntax.ID("y", _), Syntax.FSYMB("s", [Syntax.ID("0",_)], _), _)]  
*)
apply_subst subst Syntax.ID(name,pos)  = 
	 case (lookup name subst) of
	 	  NONE => Syntax.ID(name,pos)
	 	| SOME value => value 
	 	 	
apply_subst subst Syntax.FSYMB(name,terms,pos) = 
	let val terms_update = (map (apply_subst subst) terms) 
	in Syntax.FSYMB(name,terms_update,pos)
	end


(* match: expr * term (e.g., rule-head)  ->  										*)
(* 	   NONE																			*)
(* 	 | SOME subst																	*)


		
(* compose: subst_old * subst' -> subst'' 												*)




(* rewrite_cond: 																	*)
(* 		(l,r) * subst * rules-left-to-try -> 										*)
(*        NONE																		*)
(*      | SOME subst(l) * subst * (subst'',trace) * alternatives-rules-left-to-try 	*)
 
fun check_match r_subst subst [] = []
  | check_match r_subst subst (r_comp,subst)::possible_rewritings =  
	case match r r_comp of
		NONE => (check_match r_subst subst possible_rewritings)
 	  | SOME subst_cond => (compose subst subst_cond)::(check_match r_subst subst possible_rewritings)
 	   
fun rewrite_cond Syntax.COND(l,r,_) subst = []
  | rewrite_cond Syntax.COND(l,r,_) subst =
 	let val possible_rewritings = rewrite l (* this returns a list of (terms,subst) *)
 		val r_subst = (apply_subst subst r)
 	in	
 		(check_match r_subst possible_rewritings) 		
 	end     

(*															*)
(* rewrite_conds: conds * subst * trace * alternatives -> 	*)
(* 			| NONE											*)
(* 			| SOME (subst' * trace) * alternatives			*)

rewrite_conds c::cs subst = 
	let val substs = rewrite_cond(rewrite_cond c subst) 
	in
		(*  for each of the substitutions from the first condition, apply rewrite_conds cs *)
		(map (rewrite_conds cs) substs)
	end

(* apply_rule: term * rule(l,r,cs) -> 												*)
(* 			  NONE 																	*)
(* 			| SOME term *  subst' * subst'(r) * trace * cs-alternatives						*)


get_all_rhs  rhs [] = [] 
get_all_rhs  rhs (rhs_calc,subst2)::rest = 
	case match rhs rhs_calc of
		NONE => (get_all_rhs rhs rest)
	  | SOME subst => (rhs_calc, subst) :: (get_all_rhs rhs rest)
	

fun apply_rule term rule =
 let val Syntax.CRULE(lhs,rhs,conds,_)= Rule
 in 
 	case (match term lhs) of
 		NONE => NONE
 	  | SOME subst => let val all_possible_conds = rewrite_conds conds subst []
 	  				  in
 	  						let val rhs_2 = (apply_subst rhs subst) 
 	  						in (get_all_rhs rhs_2 all_possible_conds)
 	  						end
 	  				  end
 end
 		
(* rewrite: term rules -> [(terms,subst,trace)]					*)

fun rewrite_all term [] = [] 
  | rewrite_all term rule::rules = (apply_rule term rule) :: (rewrite_all term rules)

(* rewrite: term -> [(terms,subst,trace)] *)


fun rewrite term = (rewrite_all term setrules)


let val Syntax.CRULE(lhs,rhs,cond,_)= Rule 

rewrite_rule term rule = 
case match(term,rule_head) of
  SOME sub => let val sub, next rewrite_conds sub rule_conds  
  
  
(*  Invoking the semi-inverter:
    Seminv.main_semiinv [rule1,rule2] [("x",1),("y",2),("z",3)] [("add",[1,2],[])]

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


    (Seminv.rules := [rule1,rule2]; Seminv.allvars:= ["x","y","z"]; Seminv.seminv [("add",[1,2],[])])
*)  