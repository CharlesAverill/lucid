type id_type = string

type term =
  | Var of id_type
  | Lambda of (id_type * term)
  | Apply of (term * term)

let rec string_of_term t =
  match t with
  | Var n -> Printf.sprintf "\"%s\"" n
  | Lambda (n, t') -> Printf.sprintf "λ%s.%s" n (string_of_term t')
  | Apply (t1, t2) ->
      Printf.sprintf "((%s)(%s))" (string_of_term t1) (string_of_term t2)

let print_term t = print_endline (string_of_term t)
