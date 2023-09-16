%{
    open Lucid.Term_type
    open Lucid.Term
    open Lucid.Directive
    open Lucid.Store
%}

%token LAMBDA
%token DEFINITION COMPUTE CHECK
%token UNIT VOID

%token <string> IDENTIFIER

%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token DOT
%token COLON
%token ARROW
%token SET
%token EOF

%right ARROW

%start main
%type <Lucid.Directive.directives> main

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
      { Definition ($2, Term $4) }
  | DEFINITION IDENTIFIER SET term_type
      { Definition ($2, Type $4) }
  | CHECK term 
      { Check (Term $2) }
  | CHECK term_type
      { Check (Type $2) }

term :
    appTerm
      { $1 }
  | LAMBDA IDENTIFIER COLON term_type DOT term
      { Lambda ($2, $4, init_typing_context $2 $4, $6) }

term_type :
    VOID
      { VoidT }
    | UNIT 
      { UnitT }
    | term_type ARROW term_type 
      { ArrowT ($1, $3) }
    | LEFT_PARENTHESIS term_type RIGHT_PARENTHESIS
      { $2 }

appTerm :
    atomicTerm
      { $1 }
  | LEFT_PARENTHESIS RIGHT_PARENTHESIS
      { Unit }
  | appTerm atomicTerm
      { Apply ($1, $2) }

atomicTerm :
    LEFT_PARENTHESIS term RIGHT_PARENTHESIS
      { $2 }
  | IDENTIFIER
      { Var ($1) }
