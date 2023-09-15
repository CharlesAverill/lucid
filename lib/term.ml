type id_type = string

type term =
  | T
  | Var of id_type
  | Apply of (term * term)
  | Lambda of (id_type * term * term)
  | Forall of (id_type * term * term)

type theorem = {
  id: id_type;
  theorem: term;
  proof: term;
}

type func = 
| Expr of term
| Let of (id_type * term * func)
| Theorem of (theorem * func)
