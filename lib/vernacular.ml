open Term_or_type

type vernacular =
  | Definition of (string * term_or_type)
  | Compute of term_or_type
  | Check of term_or_type

type vernaculars = vernacular list
