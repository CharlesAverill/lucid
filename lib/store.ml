open Term_or_type
open Term_type
open Term

type store_range = Term of term | Type of term_type
type store = string -> store_range option

let empty_store () : store = fun _ : store_range option -> None
let _STORE : store ref = ref (empty_store ())

let get_tot_type = function 
  | TermOrTypeId id -> (
    match !_STORE id with 
    | Some (Type t) -> t
    | _ -> failwith ("Identifier " ^ id ^ " does not represent a type")
  )
  | _ -> failwith "bruh"
