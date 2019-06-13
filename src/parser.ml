open Ast
open List

exception ParserError of string
let error msg = raise (ParserError msg)

let get_token tokens =
  let token = hd !tokens in
  tokens := tl !tokens;
  token

let peek_token tokens = hd !tokens

let rec peek_n_tokens tokens = function
  | 1 -> hd !tokens
  | n -> peek_n_tokens (ref (tl !tokens)) (n - 1)

let compare_indent indent col =
  match indent with
  | InAny _ -> true
  | InEq n when col = n -> true
  | InGt n when col > n -> true
  | InGe n when col >= n -> true
  | _ -> false

let parse_string indent tokens : string =
  let token = get_token tokens in
  match token with
  | (TSTR str, col) when compare_indent indent col -> str
  | _, col -> error ("Received col: " ^ string_of_int col ^ " but expected " ^ pp_indent indent)

let parse_id indent tokens : string =
  let token = get_token tokens in
  match token with
  | (TID id, col) when compare_indent indent col -> id
  | _, col -> error ("Received col: " ^ string_of_int col ^ " but expected " ^ pp_indent indent)

let rec parse_attributes indent tokens : attribute list =
  if length !tokens < 2 then [] else
  let token = peek_n_tokens tokens 2 in
  match token with
  | (TEQ, col) when compare_indent indent col ->
    let attribute = parse_id (InGt (get_col indent)) tokens in
    let _ = get_token tokens in
    let value = parse_string (InGt col) tokens in
    let attr = Attribute(attribute, value) in
    attr :: parse_attributes indent tokens
  | _ -> []

let rec parse_element indent tokens =
  let token = get_token tokens in
  match token with
  | (TID id, col) when compare_indent indent col ->
    let attrs = parse_attributes (InGt col) tokens in
    let elems, _ = parse_elements (InGt col) tokens in
    Element(id, attrs, elems), col
  | (TSTR str, col) when compare_indent indent col -> Content(str), col
  | _, col -> error ("Received col: " ^ string_of_int col ^ " but expected " ^ pp_indent indent)

and parse_elements indent tokens =
  match hd !tokens with
  | TEOF, col -> [], col
  | tok, col when compare_indent indent col ->
    let el, col = parse_element indent tokens in
    let rst, ncol = parse_elements indent tokens in
    el :: rst, ncol
  | tok, col -> [], col

let parse_phc tokens = fst @@ parse_elements (InAny 0) tokens
