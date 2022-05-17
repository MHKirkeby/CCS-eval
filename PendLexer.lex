{
open PendParser;        (* The token type is defined in Parser.sig *)

}
rule Token = parse
    [` ` `\t`]     { Token lexbuf }     (* skip blanks *)
  | [`\n` ]        { EOL }
  | [`0`-`9` `a`-`z` `A`-`Z`]+     { ID(getLexeme lexbuf) }
  | `(`            { LPAR }
  | `)`            { RPAR }
  | `[`            { LHPAR }
  | `]`            { RHPAR }
  | `,`            { COMMA }
  | eof            { EOF }
;
