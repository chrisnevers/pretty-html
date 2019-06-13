open Ast
open Lexer
open Parser
open Dump

let run filename =
  let chan = open_in filename in
  let buf = Lexing.from_channel chan in
  let tokens = lex token buf in
  let ast = parse_phc (ref tokens) in
  dump filename (pp_elements 0 ast);
  close_in chan

let _ =
  if Array.length Sys.argv < 2
  then run "examples/basic.ph"
  else run Sys.argv.(1)
