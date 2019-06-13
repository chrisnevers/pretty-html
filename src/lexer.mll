{
  open Ast
  open Lexing

  exception LexerError of string
  let error msg = raise (LexerError msg)

  let col buf = buf.pos_cnum - buf.pos_bol

  (* Scans the input file and returns a list of annotated tokens *)
  let rec lex fn buf =
    let token = fn buf in
    match token with
    | TEOF, _ -> token :: []
    | _ -> token :: lex fn buf
}

rule token = parse
  | '\n'|'\r'|"\r\n"                { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']                      { token lexbuf }
  | "="                             { TEQ, col lexbuf.lex_start_p }
  | "'"                             { read_string (Buffer.create 17) lexbuf, col lexbuf.lex_start_p }
  | eof                             { TEOF, col lexbuf.lex_start_p }
  | [^'=''''' ''\n']+ as id         { TID id, col lexbuf.lex_start_p }

and read_string buf =
  parse
  | '''       { TSTR (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ ''' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _   { error ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
  | eof { error ("String is not terminated") }

