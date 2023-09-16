open Term_type
open Term

let rec type_of_term gamma = function
  | Unit -> UnitT
  | Var id -> (
      match gamma id with
      | Some lt -> lt
      | None -> failwith ("Tried to get type of undeclared identifier " ^ id))
  | Lambda (_, tau, inner_gamma, t) ->
      ArrowT (tau, type_of_term (merge_typing_contexts inner_gamma gamma) t)
  | Apply (t1, t2) -> (
      match (type_of_term gamma t1, type_of_term gamma t2) with
      | ArrowT (lt1, lt2), lt3 when lt1 = lt3 -> lt2
      | _ -> failwith "Ill-typed expression")
