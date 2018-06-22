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
let matching : term ->  pattern -> term list = fun t p ->
  let rec matching_aux : term -> term list -> term list = fun cur acc ->
    match cur with
    | BinOp (t1, op, t2) -> begin
      let rest = matching_aux t2 (matching_aux t1 acc) in
        match p with
        | BinOp (Any, p_op, Any) ->
            if op = p_op then cur :: rest else rest
        | BinOp (p1, p_op, Any) -> if p1 = t1 && op = p_op then
          cur :: rest else rest
        | BinOp (Any, p_op, p2) -> if p2 = t2 && op = p_op then
          cur :: rest else rest
        | BinOp (p1, p_op, p2) -> if p1 = t1 && p2 = t2 && op = p_op then
          cur :: rest else rest
        | Any -> cur :: rest
        | _ -> rest
    end
    | Var x -> if p = Var x || p = Any then cur :: acc else acc
    | Lit i -> if p = Any then cur :: acc else acc
    | _ -> acc
  in matching_aux t []


(* Return the P[x], where the substitution will happen etc *)
(* let matching : term ->  pattern -> (term -> term)  = fun t p  *)


