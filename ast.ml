type term =
  | Lit   of int
  | Var   of string
  | BinOp of term * string * term
  | Any

type pattern = term

(*
 * Initial SSReflect contextual pattern selection implementation.
 *  - TermP is the simplest c_pattern. Here the context is empty.
 *  - In pattern is the simplest contextual pattern. The context is the pattern
 *    given.
 *  - InIn(pattern, pattern) is similar, but goes one level deeper than above.
 * *)

type c_pattern =
  | TermP of pattern
  | In    of pattern
  (* | InIn  of pattern * pattern *)

let rec pretty_print : Format.formatter -> term -> unit = fun fmt t ->
  match t with
  | Lit i -> Format.fprintf fmt "%i" i
  | Var s -> Format.fprintf fmt "%s" s
  | Any -> Format.fprintf fmt "_"
  | BinOp (t1, op, t2) ->
      Format.fprintf fmt "(%a %s %a)" pretty_print t1 op pretty_print t2

(* Main matching method. *)
(*
 * let rec term_match : term -> pattern -> bool = fun t p ->
 *   match t with
 *   | BinOp (t1, op, t2) -> begin
 *     match p with
 *     | BinOp (Any, p_op, Any) -> p_op = op
 *     | BinOp (p1, p_op, Any)  -> p_op = op && term_match t1 p1
 *     | BinOp (Any, p_op, p2)  -> p_op = op && term_match t2 p2
 *     | BinOp (p1, p_op, p2) -> p_op = op && term_match t1 p1 && term_match t2 p2
 *     | Any -> true
 *     | _ -> false
 *   end
 *   | Var x -> p = Var x || p = Any
 *   | Lit i -> p = Lit i || p = Any
 *   | _  -> false (* Should not get here. Terms cannot allowed to contain Any. *)
 *)

let rec term_match : term -> pattern -> bool = fun t p ->
  match (t, p) with
  | (Any, _) -> invalid_arg "Invalid term, contains wildcard."
  | (_, Any) -> true
  | (Lit i, Lit j) -> i = j
  | (Var x, Var y) -> x = y
  | (BinOp(t1, op, t2), BinOp(p1, p_op, p2)) ->
      p_op = op && term_match t1 p1 && term_match t2 p2
  | _  -> false

(*
 * Initial term matcher - Given a term and a pattern returns all the subterms
 * that match the pattern. Note that the "Any" pattern matches all subterms
 * not just Var's and Lit's.
 * *)
let matching_list : term ->  pattern -> term list = fun t p ->
  let rec matching_aux : term -> term list -> term list = fun cur acc ->
    match cur with
    | BinOp (t1, op, t2) ->
      let rest = matching_aux t2 (matching_aux t1 acc) in
      if term_match cur p then cur :: rest else rest
    | Var x -> if p = Var x || p = Any then cur :: acc else acc
    | Lit i -> if p = Any then cur :: acc else acc
    | _ -> acc
  in matching_aux t []


(* Return the P[x], where the substitution will happen etc *)
let matching_sub : term ->  pattern -> term -> term = fun t p s ->
  let rec matching_aux : term -> term = fun cur ->
    if term_match cur p then s else match cur with
    | BinOp (t1, op, t2) ->
        let t1' = matching_aux t1 and t2' = matching_aux t2 in
      BinOp(t1', op, t2')
    | Var x -> if p = Var x || p = Any then s else cur
    | Lit i -> if p = Any then s else cur
    | _ -> cur
  in matching_aux t

(*
 * Given a term and a contextual pattern find all occurences of the subterm
 * that would be rewritten in this case.
 * *)
let rec subterm_select : term -> c_pattern -> term list = fun t c_pat ->
  match c_pat with
  | TermP pat -> matching_list t pat
  | In pat ->
      let rec first_occurence : term -> pattern -> term = fun t pat ->
        match (t, pat) with
        | (Any, _) -> Any
        | (_, Any) -> t
        | (Lit i, Lit j) -> if i = j then t else Any
        | (Var x, Var y) -> if x = y then t else Any
        | (BinOp(t1, op, t2), BinOp(p1, p_op, p2)) ->
            if term_match t pat then t else let occ1 = (first_occurence t1 p1)
            in if occ1 = Any then first_occurence t2 p2 else occ1
        | (BinOp(t1, op, t2), _) -> let occ1 = first_occurence t1 pat in
              if occ1 = Any then first_occurence t2 pat else occ1
        | _ -> Any
      in
        let occ = first_occurence t pat in if occ = Any then assert false else
        matching_list t occ
    (* | InIn (pat1, pat2) -> subterm_select t pat1 *)









