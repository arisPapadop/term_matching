type term =
  | Lit   of int
  | Var   of string
  | BinOp of term * string * term
  | Empty

type pattern = term

let rec pretty_print : Format.formatter -> term -> unit = fun fmt t ->
  match t with
  | Lit i -> Format.fprintf fmt "%i" i
  | Var s -> Format.fprintf fmt "%s" s
  | Empty -> Format.fprintf fmt "_"
  | BinOp (t1, op, t2) ->
      Format.fprintf fmt "(%a %s %a)" pretty_print t1 op pretty_print t2

(* Make term examples *)

let matching : term ->  pattern -> term list = fun t p -> assert false


(* Return the P[x], where the substitution will happen etc *)
(* let matching : term ->  pattern -> (term -> term)  = fun t p  *)


