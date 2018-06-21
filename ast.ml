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

let matching : term ->  pattern -> term list = fun t p ->
  let rec matching_aux : term -> term list -> term list = fun cur acc ->
    match cur with
    | BinOp (t1, op, t2) -> begin
      let rest = List.append (matching_aux t1 acc) (matching_aux t2 acc) in
        match p with
        | BinOp (Empty, p_op, Empty) ->
            if op = p_op then cur :: rest else rest
        | BinOp (p1, p_op, Empty) -> if p1 = t1 && op = p_op then
          cur :: rest else rest
        | BinOp (Empty, p_op, p2) -> if p2 = t2 && op = p_op then
          cur :: rest else rest
        | _ -> rest
    end
    | _  -> acc
  in matching_aux t []


(* Return the P[x], where the substitution will happen etc *)
(* let matching : term ->  pattern -> (term -> term)  = fun t p  *)


