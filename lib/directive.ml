open Term_type
open Term
open Store

type directive =
  | Definition of (id_type * store_range)
  | Compute of term
  | Check of store_range

type directives = directive list
