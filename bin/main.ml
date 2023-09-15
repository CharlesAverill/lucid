open Lucid.Input
open Lucid.Inference

(* open Lucid.Term *)
open Lucid.Directive

let _ =
  let x = parse_file Sys.argv.(1) in
  let store = empty_store () in
  eval_directives store x ""
(* print_term (eval_term x) *)

(* let _ = print_term (eval
     (Apply ((Lambda ("x", Var "x")), (Lambda ("y", Var "y"))))
   )

   let () = print_endline "Hello, World!" *)
