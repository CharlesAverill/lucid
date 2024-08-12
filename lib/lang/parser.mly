%{
    open Lucid.Term_type
    open Lucid.Term
    open Lucid.Vernacular
    open Lucid.Store
%}

%token LAMBDA
%token DEFINITION COMPUTE CHECK
%token UNIT VOID

%token <string> IDENTIFIER
%token <string> TYPE_IDENTIFIER

%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token DOT
%token COLON
%token ARROW
%token SET
%token EOF

%right ARROW

%start main
%type <Lucid.Vernacular.vernaculars> main
%type <Lucid.Vernacular.vernacular> vernacular
%type <Lucid.Term_or_type.term_or_type> term_or_type

%%

main :
    EOF
      { [] }
  | vernacular DOT main
      { $1::$3 }

vernacular : 
    COMPUTE term_or_type                                { Compute ($2) }
  | DEFINITION IDENTIFIER SET term_or_type              { Definition ($2, Term $4) }
  | CHECK term_or_type                                  { Check (Term $2) }

term_or_type : 
  // Ambiguous
  | IDENTIFIER                                            { TermOrTypeId ($1) }
  // Values
  | LEFT_PARENTHESIS term_or_type RIGHT_PARENTHESIS       { $2 }
  | LEFT_PARENTHESIS RIGHT_PARENTHESIS                    { TermValue Unit }
  | term_or_type term_or_type                             { TermApply ($1, $2) }
  | LAMBDA IDENTIFIER COLON term_or_type DOT term_or_type { TermLambda ($2, $4, $6) }
  // Types
  | VOID                                                  { TypeConstant VoidT }
  | UNIT                                                  { TypeConstant UnitT }
  | term_or_type ARROW term_or_type                       { TypeArrow ($1, $3) }

