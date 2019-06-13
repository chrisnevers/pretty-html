let dump filepath str =
  let basename = Filename.basename filepath in
  let dirname = Filename.dirname filepath in
  let filename = Filename.chop_suffix basename ".ph" in
  (* Write the {filename}.html file to the same directory *)
  let out = open_out (dirname ^ "/" ^ filename ^ ".html") in
  output_string out str;
  close_out out
