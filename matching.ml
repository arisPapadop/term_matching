open Ast

let _ =
  (* For every argument given on the command line, parse and print a term. *)
  for i = 1 to Array.length Sys.argv - 1 do
    let t = Parser.term_from_string Sys.argv.(i) in
    Format.printf "Term %i: [%a]\n%!" i pretty_print t
  done;
  (* Just some test (always run). *)
  let t = Parser.term_from_string "(a + b) * 12 + c" in
  Format.printf "Hello World! [%a]\n%!" pretty_print t
