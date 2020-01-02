{
  open Parser
  open Lexing

  exception Syntax_error

}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float = '-'? digit+ '.'? digit*
let letter = ['a'-'z' 'A'-'Z' '_' ]
let bool_id = "true\\|false"
let id = letter+



rule read =
parse
|white { read lexbuf }
|"+" { ADD }
|"%" { MOD }
|"^" { EXP }
|"log" { LOG }
|"sub" { SUBT }
|"*" { MULT }
|"/" { DIV }
|"true" { TRUE }
|"false" { FALSE }
|"sin" { SIN }
|"cos" { COS }
|"tan" { TAN }
|"sec" { SEC }
|"csc" { CSC }
|"cot" { COT }
|"==" { EQUALS }
|";" { SEMI }
|"if" { IF }
|"then" { THEN }
|"else" { ELSE }
|"let" { LET }
|"=" { EQUALS }
|"in" { IN }
|"fun" {FUN}
|"->" {ARROW}
|"(" {LPAREN}
|")" {RPAREN}
|"[" {LBRACK}
|"]" {RBRACK}
|id { ID (Lexing.lexeme lexbuf) }
|float { FLT (float_of_string (Lexing.lexeme lexbuf)) }
|eof { EOF }
|_ { raise (Syntax_error ) }