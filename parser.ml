open Ast

(** Grammar for a term at the given priority level. *)
let parser term (p : [`Atm | `Mul | `Pls]) =
  (* Atoms. *)
  | i:''[-]?[0-9]+''                when p = `Atm -> Lit(int_of_string i)
  | x:''[a-z]+''                    when p = `Atm -> Var(x)
  | "_"                             when p = `Atm -> Empty
  | "(" t:(term `Pls) ")"           when p = `Atm -> t
  (* Multiplication level. *)
  | t:(term `Mul) "*" u:(term `Atm) when p = `Mul -> BinOp(t,"*",u)
  | t:(term `Atm)                   when p = `Mul -> t
  (* Addition level. *)
  | t:(term `Pls) "+" u:(term `Mul) when p = `Pls -> BinOp(t,"+",u)
  | t:(term `Mul)                   when p = `Pls -> t

(** Entry point of the parser (term). *)
let term : term Earley.grammar =
  term `Pls

(** Exception raised on parse error (gives error position). *)
exception Parse_failure of int

(** Parses an expression from a string, may raise [Parse_failure]. *)
let term_from_string : string -> term = fun s ->
  let blank = EarleyStr.blank_regexp ''[ \t\n\r]*'' in
  try Earley.parse_string term blank s with Earley.Parse_error(_,i) ->
    raise (Parse_failure(i))
