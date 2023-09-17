open Term_type
open Term
open Directive
open Store
open Typechecker

let rec cas e1 e2 x : term =
  match e1 with
  | Var _ -> if e1 = x then e2 else e1
  | Lambda (n, g, ntype, e0) ->
      if Var n = x then Lambda (n, g, ntype, e0)
      else Lambda (n, g, ntype, cas e0 e2 x)
  | Apply (e1', e2') -> Apply (cas e1' e2 x, cas e2' e2 x)
  | _ -> e1

let rec eval_term (s : store) gamma t : term =
  (* print_endline ("Type checking term " ^ (string_of_term t)); *)
  let _typecheck = type_of_term gamma s t in
  match t with
  | Var n -> ( match s n with Some (Term e) -> e | _ -> t)
  | Lambda (x, xt, g, e) ->
      Lambda (x, xt, g, eval_term s (merge_typing_contexts g gamma) e)
  | Apply (Lambda (x, xt, g, e), y) ->
      let xt' =
        match xt with
        | TypeVar tv -> ( match s tv with Some (Type n) -> n | _ -> xt)
        | _ -> xt
      in
      if xt' = type_of_term gamma s y then
        eval_term s (merge_typing_contexts g gamma) (cas e y (Var x))
      else failwith "Ill-typed expression"
  (* | Apply (Var n, y) ->
      print_endline ("Evaluating var on left " ^ (string_of_term t));
      eval_term s gamma (Apply (eval_term s gamma (Var n), y)) *)
  | Apply (x, y) ->
      let x', y' = (eval_term s gamma x, eval_term s gamma y) in
      if x = x' && y = y' then t else eval_term s gamma (Apply (x', y'))
  | _ -> t

let eval_directive (s : store) (g : typing_context) d =
  match d with
  | Definition (name, Term expression) ->
      (* print_endline ("Updating s with " ^ name ^ " := " ^ (string_of_term expression));
         print_endline ("Updating g with " ^ name ^ " := " ^ (string_of_term_type (type_of_term g s expression))); *)
      ( update s name (Term expression),
        update g name (type_of_term g s expression) )
  | Definition (name, Type expression) -> (update s name (Type expression), g)
  | Compute expression ->
      print_term (eval_term s g expression);
      (s, g)
  | Check (Term t) ->
      Printf.printf "%s\n    : %s\n" (string_of_term t)
        (string_of_term_type (type_of_term g s t));
      (s, g)
  | Check (Type t) ->
      Printf.printf "%s\n    : Type\n" (string_of_term_type t);
      (s, g)

let rec eval_directives (s : store) (g : typing_context) d =
  match d with
  | [] -> s
  | h :: t -> (
      match eval_directive s g h with s', g' -> eval_directives s' g' t)
