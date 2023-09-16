open Term_type
open Term

type store_range = Term of term | Type of term_type
type store = id_type -> store_range option

let empty_store () : store = fun _ : store_range option -> None
let _STORE : store ref = ref (empty_store ())

let get_term_type id =
  match !_STORE id with
  | Some (Type t) -> t
  | _ -> failwith ("Failed to find type of " ^ id)
