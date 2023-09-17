open Term_type

type term =
  | Unit
  | Var of id_type
  | Lambda of (id_type * term_type * typing_context * term)
  | Apply of (term * term)

let rec string_of_term t =
  match t with
  | Unit -> "()"
  | Var n -> n
  | Lambda (n, lt, _, t') ->
      Printf.sprintf "λ%s:%s.%s" n (string_of_term_type lt) (string_of_term t')
  | Apply (t1, t2) ->
      Printf.sprintf "(%s %s)" (string_of_term t1) (string_of_term t2)

let print_term t = print_endline (string_of_term t)
