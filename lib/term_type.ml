type id_type = string

type term_type =
  | VoidT
  | UnitT
  | TypeVar of id_type
  | ArrowT of (term_type * term_type)

type typing_context = id_type -> term_type option

let merge_typing_contexts g1 g2 id =
  match g1 id with Some lt -> Some lt | None -> g2 id

let empty_typing_context () : typing_context = fun _ : term_type option -> None
let update s id value x = if x = id then Some value else s x
let init_typing_context id typ = update (empty_typing_context ()) id typ
let first_in_type t = match t with ArrowT (t, _) -> t | _ -> t

let rec string_of_term_type lt =
  match lt with
  | VoidT -> "void"
  | UnitT -> "unit"
  | TypeVar s -> s
  | ArrowT (lt1, lt2) ->
      Printf.sprintf "(%s -> %s)" (string_of_term_type lt1)
        (string_of_term_type lt2)
