open Ast
open Parser

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

let t9 = term_from_string "a + b + c"
let p9 = term_from_string "a + b + c"

(*
 * (* Helpers for testing. *)
 * let rec unzip = function
 *   | [] -> ([], [])
 *   | (a , b) :: t -> let (l1, l2) = unzip t in (a :: l1, b :: l2)
 *)

let tests1 = [(t1, p1); (t2, p2); (t3, p3); (t4, p4); (t5, p5);
              (t6, p6); (t7, p7); (t8, p8); (t9,p9)]


let tests2 = [(t1, p1, s1); (t2, p2, s2); (t3, p3, s3); (t4, p4, s4)]


(* This is the most basic context selection test. The context is (a + b)
 * and that is the result we expect.
 * *)
let pat1   = term_from_string "X + c"
let c_pat1 = InTerm ((MetaVar "X"), pat1)
let term1  = term_from_string "(a + b) + c"

(* This test asserts that all instances of the fisrt expression selected are
 * identified.
 * *)
let pat2   = term_from_string "X + _"
let c_pat2 = InTerm ((MetaVar "X"), pat2)
let term2  = term_from_string "((a + b) + c) + (a + b)"

(* This tests asserts that metavariables on the RHS of BinOps work as
 * expected,
 * *)
let pat3   = term_from_string "a + X"
let c_pat3 = InTerm ((MetaVar "X"), pat3)
let term3  = term_from_string "((a + b) + c) + (a + b)"

(* Gives rise to Not_found exception
 *
 * let pat4   = term_from_string "X * c"
 * let c_pat4 = InTerm ((MetaVar "X"), pat4)
 * let term4  = term_from_string "(a + b) + c"
 *)

let pat4_1 = term_from_string "_ * _"
let pat4_2 = term_from_string "a + X"
let c_pat4 = InInTerm (pat4_1, MetaVar "X", pat4_2)
let term4  = term_from_string "a + (b + c * d)"



let tests3 = [(c_pat1, term1); (c_pat2, term2); (c_pat3, term3);
              (c_pat4, term4)]
