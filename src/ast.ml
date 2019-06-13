(** Token types and functions **)

type token =
  | TID of string
  | TSTR of string
  | TEQ
  | TEOF

let pp_token = function
  | TID id -> "TID " ^ id
  | TSTR str -> "TSTR " ^ str
  | TEQ -> "TEQ"
  | TEOF -> "TEOF"

let pp_tokens tokens = String.concat "\n" @@ List.map pp_token tokens

let pp_atoken = function (token, col) -> pp_token token ^ " : " ^ string_of_int col

let pp_atokens atokens = String.concat "\n" @@ List.map pp_atoken atokens

(** AST types and functions **)

type attribute =
  | Attribute of string * string

let pp_attribute = function
  | Attribute (key, value) -> key ^ "=\"" ^ value ^ "\""

let pp_attributes attrs = String.concat " " @@ List.map pp_attribute attrs

type tag = string

type element =
  | Element of tag * attribute list * element list
  | Content of string

let rec pp_indent = function
  | 0 -> ""
  | n -> " " ^ pp_indent (n - 1)

let rec pp_element indent el =
  pp_indent indent ^
  match el with
  (* Ex: <h1>Hello World!</h1> *)
  | Element(tag, [], [Content s]) -> "<" ^ tag ^ ">" ^ s ^ "</" ^ tag ^ ">"
  (* Ex:
    <div>
      <h1>Hello World!</h1>
    </div>
  *)
  | Element(tag, [], eles) -> "<" ^ tag ^ ">\n" ^ pp_elements (indent + 2) eles ^"\n" ^ pp_indent indent ^ "</" ^ tag ^ ">"
  (* Ex: <img src="blah.png" /> *)
  | Element(tag, attrs, []) -> "<" ^ tag ^ " " ^ pp_attributes attrs ^ "/>"
  (* Ex:
    <div class= "col-sm-12">
      <h1>Hello World!</h1>
      <img src= "../images/dog.png"/>
    </div>
  *)
  | Element(tag, attrs, eles) -> "<" ^ tag ^ " " ^ pp_attributes attrs ^ ">\n" ^ pp_elements (indent + 2) eles ^"\n" ^ pp_indent indent ^ "</" ^ tag ^ ">"
  (* Ex: Hello World! *)
  | Content(str) -> str

and pp_elements indent eles = String.concat "\n" @@ List.map (pp_element indent) eles

(** Indentation types and functions **)

type indent =
  | InEq of int
  | InGt of int
  | InGe of int
  | InAny of int

let get_col = function
  | InEq i | InGt i | InGe i | InAny i -> i

let pp_indent = function
  | InEq i -> "eq " ^ string_of_int i
  | InGt i -> "gt " ^ string_of_int i
  | InGe i -> "ge " ^ string_of_int i
  | InAny i -> "any " ^ string_of_int i
