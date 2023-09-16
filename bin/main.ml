open Lang.Input
open Lucid.Inference
open Lucid.Term_type
open Lucid.Store

let _ =
  let x = parse_file Sys.argv.(1) in
  let gamma = empty_typing_context () in
  eval_directives !_STORE gamma x ""
