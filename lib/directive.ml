open Term

type store = id_type -> term option

let empty_store () : store = fun _ -> None
let update s id value : store = fun x -> if x = id then Some value else s x

type directive = Definition of (id_type * term) | Compute of term
type directives = directive list
