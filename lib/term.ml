open Term_type

type term =
  | Unit
  | Var of id_type
  | Lambda of (id_type * term_type * typing_context * term)
  | Apply of (term * term)

let rec type_of_term gamma = function
  | Unit -> UnitT
  | Var id -> (
      match gamma id with
      | Some lt -> lt
      | None -> failwith ("Failed to get type of " ^ id))
  | Lambda (_, tau, inner_gamma, t) ->
      ArrowT (tau, type_of_term (merge_typing_contexts inner_gamma gamma) t)
  | Apply (t1, t2) -> (
      match (type_of_term gamma t1, type_of_term gamma t2) with
      | ArrowT (lt1, lt2), lt3 when lt1 = lt3 -> lt2
      | _ -> failwith "Ill-typed expression")

let rec string_of_term t =
  match t with
  | Unit -> "()"
  | Var n -> n
  | Lambda (n, lt, _, t') ->
      Printf.sprintf "λ%s:%s.%s" n (string_of_term_type lt) (string_of_term t')
  | Apply (t1, t2) ->
      Printf.sprintf "((%s)(%s))" (string_of_term t1) (string_of_term t2)

let print_term t = print_endline (string_of_term t)
