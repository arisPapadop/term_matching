open Ast
open Parser

(*
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

(*
 * A bunch of handwritten tests to check the first implementation of the term
 * matcher.
 *)

(* The whole term matches*)
let t1 = term_from_string "1 * 2 + x"
let p1 = term_from_string "1 * 2 + _"

(* Everything matches. *)
let t2 = term_from_string "1 + 2 + 3 + 4"
let p2 = term_from_string "_"

(* The second operand matches. *)
let t3 = term_from_string "(1 + 2) + (x + y)"
let p3 = term_from_string "x + y"

(* Nothing matches, because operations are left-associative. *)
let t4 = term_from_string "1 + 2 + x + y"
let p4 = term_from_string "x + y"

(* Nothing matches, because multiplication has lower precedence.  *)
let t5 = term_from_string "1 * 2 + x "
let p5 = term_from_string "2 + x"

let tests = [(t1, p1); (t2, p2); (t3, p3)]

let _ =  List.map (Format.printf "Term: [%a]\n%!" pretty_print) (matching t5 p5)

