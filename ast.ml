type term =
  | Lit   of int
  | Var   of string
  | BinOp of term * string * term
  | Any

type pattern = term

let rec pretty_print : Format.formatter -> term -> unit = fun fmt t ->
  match t with
  | Lit i -> Format.fprintf fmt "%i" i
  | Var s -> Format.fprintf fmt "%s" s
  | Any -> Format.fprintf fmt "_"
  | BinOp (t1, op, t2) ->
      Format.fprintf fmt "(%a %s %a)" pretty_print t1 op pretty_print t2

(* Initial term matcher *)
let rec nested_match : term -> pattern -> bool = fun t p ->
  match t with
  | BinOp (t1, op, t2) -> begin
     match p with
      | BinOp (Any, p_op, Any) -> p_op = op
      | BinOp (p1, p_op, Any)  -> p_op = op && nested_match t1 p1
      | BinOp (Any, p_op, p2)  -> p_op = op && nested_match t2 p2
      | BinOp (p1, p_op, p2)   -> p_op = op && nested_match t1 p1
                                            && nested_match t2 p2
      | Any -> true
      | _ -> false
  end
  | Var x -> p = Var x || p = Any
  | Lit i -> p = Any
  | _  -> false (* Should not get here. *)

let matching : term ->  pattern -> term list = fun t p ->
  let rec matching_aux : term -> term list -> term list = fun cur acc ->
    match cur with
    | BinOp (t1, op, t2) ->
      let rest = matching_aux t2 (matching_aux t1 acc) in
      if nested_match cur p then cur :: rest else rest
    | Var x -> if p = Var x || p = Any then cur :: acc else acc
    | Lit i -> if p = Any then cur :: acc else acc
    | _ -> acc
  in matching_aux t []


(* Return the P[x], where the substitution will happen etc *)
(* let matching : term ->  pattern -> (term -> term)  = fun t p  *)


