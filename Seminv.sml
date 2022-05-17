

(************************************************************************)
(* The following semi-inverter is an implementation of the semi-inversion
 algorithm presented in [1]. It is only intended to be used for CCSs.   *) 


(*=============================================================================


    File:       semiinv.sml

    Authors:    Maja Hanne Kirkeby, Roskilde University, Roskilde, Denmark 
                kirkebym at acm dot org

    Date:       19-06-2020
    
    Version:    1.0
    
    Copyright:  2020, Maja H. Kirkeby, Robert Glück 

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

=============================================================================*)
structure Seminv =
struct



(************************************************************************)
(*  load of libraries and definitions of execption and global variables *)
(************************************************************************)
(*
load "Int";
load "Real";
load "Syntax";
*)

exception internalError;

val allvars = ref []
val rules = ref []

(************************************************************************)
(* functions for list and set  manipulations				*)
(************************************************************************)

(* (remove x ys) if element x is in list ys, it removes an element x    *)
(* from list y  and return the new list, otherwise, it returns ys.      *)
fun remove x [] = []
 | remove x (y::ys) = if x=y then ys else y::(remove x ys);

(* (setminus XS YS) If XS and YS are a non-repeating list, then YS \ XS	*)
(* if YS is repeating it removes all elements of YS occurring in XS     *)
fun setminus [] ys = ys
 | setminus (x::xs) ys = (setminus xs (remove x ys));

(* exists check								*)
fun lookup x [] = false
 | lookup x (y::ys) = if x=y then true else (lookup x ys);

(* subset check								*)
fun subset [] ys = true
  | subset (x::xs) ys = if (lookup x ys) then (subset xs ys) else false;


(* union of list and non-repeating list *)
fun union [] vars = vars
 | union (x::xs) vars = if (lookup x vars) then (union xs vars) else
 (union xs (x::vars));


(* count number of t occurrences in a list				*)
fun count t alist = foldr  (fn (x,y) => (if x=t then y+1 else y)) 0 alist;

(* retrieves indices for true in boollist				*)
fun index boollist =
     #1(foldr (fn (x,(is,i)) => if x=true
     	      	  	     	then ((i::is),(i-1))
				else (is,(i-1)))
    	      ([],(List.length(boollist)))
	      boollist
	);

(************************************************************************)
(* functions for labeling of function symbols with semi-inversion       *)
(************************************************************************)
fun in_listtoname [] = ""
 | in_listtoname [x] = (Int.toString x)
 | in_listtoname (x::xs) = (Int.toString x)^","^(in_listtoname xs);

fun listtoname x = "{"^(in_listtoname x)^"}";

fun rename n1 I O =
    let val Iname = listtoname I
    	val Oname = listtoname O
    in
	"_"^n1^"_"^Iname^Oname
    end;



(************************************************************************)
(* functions for decoding labeled function symbols into pending tasks 	*)
(************************************************************************)
fun in_string_to_set [] set = (set, [])
| in_string_to_set (#"}"::ss) set = (set,ss)
| in_string_to_set (s::ss) set =
    case s of
      #"," => (in_string_to_set ss set)
    | c => if Char.isDigit(c)
      	   then
	   let val (SOME n) = (Int.fromString(Char.toString(c)))
	    in
	   (in_string_to_set ss (set@[n]))
	   end
      	   else raise internalError;

fun string_to_set charlist = (in_string_to_set charlist []);

fun in_string_to_name (#"_"::ss) set = (set,ss)
 | in_string_to_name (s::ss) set =
   let val (set1,ss1) = (in_string_to_name ss set) in 
    (s::set1,ss1)
    end;

fun string_to_name charlist =
    let val (cname,rest) = (in_string_to_name charlist [])
    in
	(implode(cname),rest)
    end;

fun decode n =
    let val (#"_"::namelist) = explode(n)
    in let val (name,(#"{"::rest1)) = (string_to_name namelist)
     	in let val (I,(#"{"::rest2)) = (string_to_set rest1)
	   in
	     let val O = #1(string_to_set rest2) in
	     (name, I, O)
	     end
	   end
	end
    end;

fun decode_name(Syntax.COND(Syntax.FSYMB(n,_,_),_,_)) =
    decode n;



(************************************************************************)
(* functions for splitting a list according to a list of sorted indices *)
(************************************************************************)

fun in_split([],_,_,a,b) = (a,b)
 | in_split((x::xs),[],n,a, b) = (a,b@(x::xs))
 | in_split((x::xs),(i::iis),n,a, b)  =
   if i=n then in_split(xs,iis,(n+1),(a@[x]),b)
      	  else in_split(xs,(i::iis),(n+1),a,(b@[x]));

(* (split xs I) takes a list xs and a sorted list of indices and split  *)
(* xs into two lists ys and zs s.t. ys contains the elements of xs with *)
(* indices in I and zs contains the rest.	    	     		*)

fun split xs I = in_split(xs, I, 1, [], []) ; 

(************************************************************************)
(* functions that given a function name f retrieves all rules defining f*)
(* NB!: uses the global variable "rules"				*)
(************************************************************************)
fun is_f_rule f (Syntax.CRULE(Syntax.FSYMB(n,_,_),_,_,_)) = 
if f=n then true else false;

fun get_indications [] [] = []
| get_indications (t::ts) (r::rs) =
   if t then (r::(get_indications ts rs)) else (get_indications ts rs);

fun get_f_rules f =
    let val frule_indications = (map (is_f_rule f) (!rules))  in
    	(get_indications frule_indications (!rules))
	end;


(************************************************************************)
(* functions that retrieve variables and recognize known parameters	*)
(* NB!: uses the global variable "allvars"				*)
(* example: allvars = ["x","y"]; getvars(lhs) = ["y"] 			*)
(************************************************************************)

fun getvars(Syntax.ID(s,_)) = if (lookup s (!allvars)) then [s] else []
 | getvars(Syntax.FSYMB(_,termlist,_)) = getvars_list(termlist)

and getvars_list([]) = []
  | getvars_list((t::ts)) = getvars(t)@getvars_list(ts);

fun getvars_cond (Syntax.COND(t1,t2,_)) = union (getvars(t1)) (getvars(t2));

fun isknownparm knownvars term = (subset (getvars(term)) knownvars);

(* known_parms t v creates a new list u of same lengt as t where each
position in u is either true -indicating that the term in the same position
in t is a known parameter- or false -indicating that it is not known.   *)
fun known_parms termlist knownvars =  (map (isknownparm knownvars) termlist);


(************************************************************************)
(* frac_inv(l,r,I,O) inverts a rule fraction l->r according to I and O  *)
(************************************************************************)

fun frac_inv(Syntax.FSYMB(n1,t1,_),Syntax.FSYMB(n2,t2,_), I, O) =
    let val m1 = (rename n1 I O)
    	val (t1I,t1O) = (split t1 I)
	val (t2I,t2O) = (split t2 O)
	val dummypos = (0,0)
    in
    (Syntax.FSYMB(m1,(t1I@t2I),dummypos), Syntax.FSYMB(n2,(t1O@t2O),dummypos)) 
    end;


(************************************************************************)
(* function percent in Figure 5 [1]					*)
(************************************************************************)

fun percent knownvars (Syntax.COND(Syntax.FSYMB(_,t1,_),Syntax.FSYMB(_,t2,_),_)) =
    let val I_size = (count true (known_parms t1 knownvars))
    	val O_size = (count true (known_parms t2 knownvars))
    in
	Real.fromInt(I_size + O_size) / Real.fromInt(List.length(t1)+List.length(t2))
    end;

(************************************************************************)
(* function maxPercent in Figure 5 [1]					*)
(************************************************************************)
fun maxPercent reallist =
    let fun step(x, (y, ypos, count)) =
    	    if x>y then (x,count,(count+1)) else (y,ypos,(count+1))
    in
	 #2(foldl step (~1.0,~1,1) reallist)
    end ;

(************************************************************************)
(* function heuristic in Figure 5 [1]					*)
(************************************************************************)
fun heuristic [] _ = []
  | heuristic condlist vars =
     let val percentlist = (map (percent vars) condlist)
     in
         let val index_i = (maxPercent percentlist)
         in
	     let val ([c_i],rest) = (split condlist [index_i])
             in
	     (c_i :: (heuristic rest (union (getvars_cond(c_i)) vars)))
	     end
	 end
      end;

(************************************************************************)
(* function localinvc in Figure 4 [1]					*)
(************************************************************************)

fun localinvc [] _ = []
 | localinvc (c::cs) knownvars =
    let val (Syntax.COND(Syntax.FSYMB(n1,t1,p1),Syntax.FSYMB(n2,t2,p2),p)) = c in
    	let val I = index (known_parms t1 knownvars)
    	    val O = index (known_parms t2 knownvars)
    	in
	    let val (lhs,rhs) = frac_inv(Syntax.FSYMB(n1,t1,p1),Syntax.FSYMB(n2,t2,p2), I, O)
	    in
(*		((Syntax.COND(lhs,rhs,(0,0))) :: (localinvc cs (updateVars knownvars c)))	 *)
		((Syntax.COND(lhs,rhs,(0,0))) :: (localinvc cs (union (getvars_cond(c)) knownvars)))		
	    end
	end
    end;


(************************************************************************)
(* function localinv in Figure 4 [1]					*)
(************************************************************************)

fun localinv_semi I O Rule =
    let val Syntax.CRULE(lhs,rhs,cond,_)= Rule
    in
	let val	(lhs_i,rhs_i) = frac_inv(lhs,rhs,I,O)	
    	in
		let val knownVars = getvars(lhs_i)
		in
			let val cond_reordered = (heuristic cond knownVars)
	      		in
				let val cond_i = localinvc cond_reordered knownVars
		    		in
		    		Syntax.CRULE(lhs_i,rhs_i,cond_i,(0,0))
				end
	  		end
		end
	end
   end;



(************************************************************************)
(* function getdep in Figure 3 [1]					*)
(************************************************************************)
fun getdep [] = []
 | getdep (r::rs) =
   (* retrieve the list of dependencies from the conditions *)
   let val Syntax.CRULE(_,_,conds,_)= r
   in 
	((map decode_name conds)@(getdep rs))
   end;
	

(************************************************************************)
(* The main loop of the semi-inversion algorithm [1] (Figure 3)         *)
(************************************************************************)
fun seminv [] done = []
 | seminv ((f,I,O)::xs) done =
   let val frules_original = (get_f_rules f)
   in
     let val frules_inverted = (map (localinv_semi I O) frules_original)
     in
        let val newdep = (setminus  [(f,I,O)] (setminus done (getdep frules_inverted)))
	in
	  (union frules_inverted (seminv (union newdep xs) ((f,I,O)::done)))
	end
     end
   end;
(* seminv [(add,[1],[1])] [];  *)

fun clean (a,_) = a; 

fun main_semiinv systemrules systemvars pend =
    let val cleaned_systemvars = (map clean systemvars)
    in
    (allvars := cleaned_systemvars; rules := systemrules;
    (seminv pend []))
    end;



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




end