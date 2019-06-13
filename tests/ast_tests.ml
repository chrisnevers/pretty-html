open OUnit
open Lexer
open Parser
open Ast

let parse_str str =
  let buf = Lexing.from_string str in
  let tokens = lex token buf in
  parse_phc (ref tokens)

let elem_no_attribute_no_children_test () =
  let actual_ast = parse_str "h1 'Hello World!'" in
  let expected_ast = [Element("h1", [], [Content "Hello World!"])] in
  assert_equal actual_ast expected_ast;
  let actual_str = pp_elements 0 actual_ast in
  let expected_str = "<h1>Hello World!</h1>" in
  assert_equal actual_str expected_str

let elem_no_attribute_children_test () =
  let actual_ast = parse_str "  div\n    h1 'Hello World!'" in
  let actual_str = pp_elements 0 actual_ast in
  let expected_str = "<div>\n  <h1>Hello World!</h1>\n</div>" in
  assert_equal actual_str expected_str

let elem_no_children_test () =
  let actual_ast = parse_str "img src='shrek.png'" in
  let actual_str = pp_elements 0 actual_ast in
  let expected_str = "<img src=\"shrek.png\"/>" in
  assert_equal actual_str expected_str

let elem_attribute_children_test () =
  let actual_ast = parse_str "img src='shrek.png' 'This is shrek'\n  'Also works'\n  p 'hello'" in
  let actual_str = pp_elements 0 actual_ast in
  let expected_str = "<img src=\"shrek.png\">\n  This is shrek\n  Also works\n  <p>hello</p>\n</img>" in
  assert_equal actual_str expected_str

let content_test () =
  let actual_ast = parse_str "'I am shrek'" in
  let actual_str = pp_elements 0 actual_ast in
  let expected_str = "I am shrek" in
  assert_equal actual_str expected_str

let suite =
  "Tests" >:::
  [
    "elem no attribute w.o children" >:: elem_no_attribute_no_children_test;
    "elem no attribute w children" >:: elem_no_attribute_children_test;
    "elem w attribute and children" >:: elem_attribute_children_test;
    "elem no children" >:: elem_no_children_test;
    "content" >:: content_test;
  ]

let run_ast_tests () = run_test_tt_main suite
