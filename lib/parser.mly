%{
    open Term
    open Directive
%}

/* Keyword tokens */
%token LAMBDA
%token DEFINITION COMPUTE

/* Identifiers tokens */
%token <string> IDENTIFIER

/* Symbolic tokens */
%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token DOT
%token SET
%token EOF

%start main
%type <Directive.directives> main

%%

main :
    EOF
      { [] }
  | command DOT main
      { $1::$3 }

command :
    COMPUTE term
      { Compute ($2) }
  | DEFINITION IDENTIFIER SET term
      { Definition ($2, $4) }

term :
    appTerm
      { $1 }
  | LAMBDA IDENTIFIER DOT term
      { Lambda ($2, $4) }

appTerm :
    atomicTerm
      { $1 }
  | appTerm atomicTerm
      { Apply ($1, $2) }

atomicTerm :
    LEFT_PARENTHESIS term RIGHT_PARENTHESIS
      { $2 }
  | IDENTIFIER
      { Var ($1) }
