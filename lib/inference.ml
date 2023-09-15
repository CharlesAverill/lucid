open Term

let judgement (e : term) (t : term) = match e with 
| P -> t = T 
| _ -> false

