open Term_type
open Term
open Store

let reduce_type (st : store) t =
  match t with TypeVar s -> st s | _ -> Some (Type t)

let rec type_of_term gamma (s : store) = function
  | Unit -> UnitT
  | Var id -> (
      match gamma id with
      | Some lt -> lt
      | None ->
          failwith
            ("Typechecker fail: Tried to get type of undeclared identifier "
           ^ id))
  | Lambda (_, tau, inner_gamma, t) ->
      ArrowT (tau, type_of_term (merge_typing_contexts inner_gamma gamma) s t)
  | Apply (t1, t2) -> (
      let t1' = reduce_type s (type_of_term gamma s t1) in
      let t2' = reduce_type s (type_of_term gamma s t2) in
      match (t1', t2') with
      | Some (Type (ArrowT (lt1, lt2))), Some (Type lt3)
        when reduce_type s lt1 = reduce_type s lt3 ->
          lt2
      | Some (Type t1''), Some (Type t2'') ->
          failwith
            (Printf.sprintf
               "Typechecker fail: Ill-typed expression (%s applied to %s)"
               (string_of_term_type t1'') (string_of_term_type t2''))
      | _, _ -> failwith "Typechecker fail: Couldn't get types of terms")
