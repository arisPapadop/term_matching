open Ast
open Tests

(* Command Line testing.
 *
 * let _ =
 *   (* For every argument given on the command line, parse and print a term. *)
 *   for i = 1 to Array.length Sys.argv - 1 do
 *     let t = Parser.term_from_string Sys.argv.(i) in
 *     Format.printf "Term %i: [%a]\n%!" i pretty_print t
 *   done;
 *   (* Just some test (always run). *)
 *   let t = Parser.term_from_string "(a + b) * 12 + c" in
 *   Format.printf "[%a]\n%!" pretty_print t
 *)

let _ =
  Format.printf "Matching Tests. \n%!" ;
  for i = 0 to List.length tests1 -1 do
  let (t, p) = List.nth tests1 i in
  Format.printf "Test %i \n%!" i;
  Format.printf "Term: [%a] - " pretty_print t;
  Format.printf "Pattern: [%a]\n%!Matches \n%!" pretty_print p;
  List.map (Format.printf "[%a]\n%!" pretty_print) (matching_list t p);
  Format.print_string "\n"
  done


let _ =
  Format.printf "Substitution Tests. \n%!" ;
  for i = 0 to List.length tests2 -1 do
  let (t, p, s) = List.nth tests2 i in
  Format.printf "Test %i \n%!" i;
  Format.printf "Term: [%a] - " pretty_print t;
  Format.printf "Pattern: [%a] - " pretty_print p;
  Format.printf "Substitute with : [%a] \n%!" pretty_print s;
  Format.printf "Result [%a]\n%!" pretty_print (matching_sub t p s);
  Format.print_string "\n"
  done


let _ =
  Format.printf "Context Selection Tests. \n%!" ;
  for i = 0 to List.length tests3 -1 do
  let (c_pat, t) = List.nth tests3 i in
  Format.printf "Test %i \n%!" i;
  Format.printf "Term: [%a] - " pretty_print t;
  Format.printf "Pattern: [%a] - " pretty_print_context c_pat;
  List.map (Format.printf "[%a]\n%!" pretty_print) (subterm_select t c_pat);
  Format.print_string "\n"
  done
