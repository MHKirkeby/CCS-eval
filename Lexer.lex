{
 open Lexing;

 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 val currentLine = ref 1
 val lineStartPos = ref [0]

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
				(!currentLine)
				(!lineStartPos)

 and getLineCol pos line (p1::ps) =
       if pos>=p1 then (line, pos-p1)
       else getLineCol pos (line-1) ps
   | getLineCol pos line [] = raise LexicalError ("",(0,0))

 fun lexerError lexbuf s = 
     raise LexicalError (s, getPos lexbuf)

 fun keyword (s, pos) =
     case s of
         "RULES"         => Parser.R pos
       | "VAR"           => Parser.V pos
       | "CONDITIONTYPE" => Parser.CONDT pos
       | "SEMI-EQUATIONAL" => Parser.SEMIEQ pos
       | "JOIN"            => Parser.JOIN pos
       | "ORIENTED"        => Parser.ORIENTED pos
       | "->"              => Parser.RARROW pos
       | "=="              => Parser.EQEQ pos
       | _              => Parser.ID (s, pos)

 }

rule Token = parse
    [` ` `\t` `\r`]+    { Token lexbuf } (* whitespace *)
  | [`\n` `\012`]       { currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          Token lexbuf } (* newlines *)
			  (* remove COMMENT section *)
  | "(COMMENT" ([^`)`])* ")"
  { Parser.COMMENT (getLexeme lexbuf, getPos lexbuf)  } (* comment *)			  
  | [`-` `a`-`z` `A`-`Z` `0`-`9` `=` `#`] [`a`-`z` `A`-`Z` `0`-`9` `_` `-` `>` `=` `#` ]* 
                        { keyword (getLexeme lexbuf,getPos lexbuf) }
  | `(`                 { Parser.LPAR (getPos lexbuf) }
  | `)`                 { Parser.RPAR (getPos lexbuf) }
  | `|`                 { Parser.BAR  (getPos lexbuf) }
  | `,`                 { Parser.COMMA (getPos lexbuf) }
  | eof                 { Parser.EOF (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

;
