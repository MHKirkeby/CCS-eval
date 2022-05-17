
(* original structure
  ctrs     ::= (CONDITIONTYPE ctype)
  	       [(VAR idlist)]
	       (RULES rulelist)
	       [(COMMENT string)]
  ctype    ::= SEMI-EQUATIONAL | JOIN | ORIENTED
  idlist   ::= ε | id idlist
  rulelist ::= ε | rule rulelist
  rule     ::= term '->' term | term '->' term '|' condlist
  condlist ::= cond | cond, condlist
  cond     ::= term '==' term
  term     ::= id | id() | id(termlist)
  termlist ::= term | term, termlist
*)

(* abs structure using lists when possible

ctrs     ::= (CONDITIONTYPE ctype) (VAR id list) (RULES rule :: rule list) (COMMENT string)
  ctype    ::= SEMI-EQUATIONAL | JOIN | ORIENTED
  rule     ::= term '->' term |  term '->' term '|' cond :: cond list
  cond     ::= term '==' term
  term     ::= id | id() | id(term :: term list)  
*)


(* simplified abs structure 

ctrs     ::= (CONDITIONTYPE ctype) (VAR id list) (RULES rule list) (COMMENT string)
  ctype    ::= SEMI-EQUATIONAL | JOIN | ORIENTED
  rule     ::= term '->' term '|' cond list
  cond	   ::= term '==' term 
  term     ::= id | id() | id(term list)  
*)


structure Syntax =
struct
	type pos = int * int (* position in program (line,column) *)
	type Id = string * pos

	datatype Comment = COMMENT of string * pos 

	datatype Ctype = SEMIEQUATIONAL of pos | JOIN of pos | ORIENTED of pos


	 
   	 datatype Term = ID of Id | FSYMB of string * Term list * pos

	 datatype Cond = COND of Term * Term * pos

	 datatype Rule =  CRULE of Term * Term * Cond list * pos

	 type Ctrs = Ctype * Id list * Rule list * Comment
end