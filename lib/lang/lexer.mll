{
    open Parser
    open Lexing
    exception UnexpectedCharacter
}

let ident = ['A'-'Z' 'a'-'z' '_'] (['A'-'Z' 'a'-'z' '0'-'9' '_'])* 

rule token = parse
    | [' ' '\t' '\r' '\n']          { token lexbuf }
    | '('                           { LEFT_PARENTHESIS }
    | ')'                           { RIGHT_PARENTHESIS }
    | '\\' | "λ"                    { LAMBDA }
    | '.'                           { DOT }
    | ':'                           { COLON }
    | "->"                          { ARROW }
    | ":="                          { SET }
    | "(*"                          { comment lexbuf }
    | "void"                        { VOID }
    | "unit"                        { UNIT }
    | "Definition"                  { DEFINITION }
    | "Compute"                     { COMPUTE }
    | "Check"                       { CHECK }
    | '\'' ident                    { TYPE_IDENTIFIER (lexeme lexbuf) }
    | ident                         { IDENTIFIER (lexeme lexbuf) }
    | eof                           { EOF }
    | _ { raise UnexpectedCharacter }

and comment = parse
    | "*)"                          { token lexbuf }
    | eof                           { failwith "Unexpected EOF" }
    | _                             { comment lexbuf }
