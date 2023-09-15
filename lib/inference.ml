open Term
open Directive

let rec cas e1 e2 x : term =
  match e1 with
  | Var _ -> if e1 = x then e2 else e1
  | Lambda (n, e0) ->
      if Var n = x then Lambda (n, e0) else Lambda (n, cas e0 e2 x)
  | Apply (e1', e2') -> Apply (cas e1' e2 x, cas e2' e2 x)

let rec eval_term s t : term =
  match t with
  | Var n -> ( match s n with Some e -> e | None -> t)
  | Lambda (x, e) -> Lambda (x, eval_term s e)
  | Apply (Lambda (x, e), y) -> eval_term s (cas e y (Var x))
  | Apply (Var n, y) -> eval_term s (Apply (eval_term s (Var n), y))
  | _ -> t

let eval_directive s d =
  match d with
  | Definition (name, expression) -> update s name expression
  | Compute expression ->
      print_term (eval_term s expression);
      s

let rec eval_directives s d =
  match d with [] -> s | h :: t -> eval_directives (eval_directive s h) t
