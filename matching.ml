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
let s1 = term_from_string "y" (* Result: y. *)

let t2 = term_from_string "1 * 2 + x"
let p2 = term_from_string "x"
let s2 = term_from_string "y" (* Result 1 * 2 + y. *)

(* Everything matches. *)
let t3 = term_from_string "1 + 2 + 3 + 4"
let p3 = term_from_string "_"
let s3 = term_from_string "2" (* Result 2 *)
(* This could change to give '2 + 2 + 2 + 2'. *)

(* The second operand matches. *)
let t4 = term_from_string "(1 + 2) + (x + y)"
let p4 = term_from_string "x + y"
let s4 = term_from_string "y + x" (* Result: (1 + 2) + (y + x). *)

(* Nothing matches, because operations are left-associative. *)
let t5 = term_from_string "1 + 2 + x + y"
let p5 = term_from_string "x + y"

(* Nothing matches, because multiplication has lower precedence.  *)
let t6 = term_from_string "1 * 2 + x "
let p6 = term_from_string "2 + x"

(* Nothing matches, because multiplication has lower precedence.  *)
let t7 = term_from_string "z * y + x "
let p7 = term_from_string "z * _ + x"

(* Nothing matches, because multiplication has lower precedence.  *)
let t8 = term_from_string "2 + x "
let p8 = term_from_string "_ * x"

(*
 * (* Helpers for testing. *)
 * let rec unzip = function
 *   | [] -> ([], [])
 *   | (a , b) :: t -> let (l1, l2) = unzip t in (a :: l1, b :: l2)
 *)



let tests1 = [(t1, p1); (t2, p2); (t3, p3); (t4, p4); (t5, p5);
              (t6, p6); (t7, p7); (t8, p8)]

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

let tests2 = [(t1, p1, s1); (t2, p2, s2); (t3, p3, s3); (t4, p4, s4)]

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

(* let c_pat1 = (In term_from_string "_ + _") *)

let tests3 = []

let _ =
  Format.printf "Context Selection Tests. \n%!" ;
  for i = 0 to List.length tests3 -1 do
  Format.print_string "\n"
  done
