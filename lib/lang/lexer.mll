{
    open Parser
    open Lexing
    exception UnexpectedCharacter
}

rule token = parse
    | [' ' '\t' '\r' '\n']          { token lexbuf }
    | '('                           { LEFT_PARENTHESIS }
    | ')'                           { RIGHT_PARENTHESIS }
    | '\\' | "λ"                    { LAMBDA }
    | '.'                           { DOT }
    | ':'                           { COLON }
    | "->"                          { ARROW }
    | "void"                        { VOID }
    | "unit"                        { UNIT }
    | "Definition"                  { DEFINITION }
    | "Compute"                     { COMPUTE }
    | "Check"                       { CHECK }
    | ['A'-'Z' 'a'-'z' '_'] (['A'-'Z' 'a'-'z' '0'-'9' '_'])*       
                                    { IDENTIFIER (lexeme lexbuf) }
    | ":="                          { SET }
    | "(*"                          { comment lexbuf }
    | eof                           { EOF }
    | _ { raise UnexpectedCharacter }

and comment = parse
    | "*)"                          { token lexbuf }
    | _                             { comment lexbuf }
    | eof                           { failwith "Unexpected EOF" }
