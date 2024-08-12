open Term
open Term_type

type term_or_type = 
  | TermOrTypeId of string
  | TermValue of term
  | TermApply of (term_or_type * term_or_type)
  | TermLambda of (string * term_or_type * term_or_type)
  | TypeConstant of term_type
  | TypeArrow of (term_or_type * term_or_type)
