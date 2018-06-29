type term =
  | Lit     of int
  | Var     of string
  | BinOp   of term * string * term
  | MetaVar of string
  | Any

type pattern = term

type c_pattern =
  | Term   of pattern
  | InTerm of term * term (* Single level of depth. *)

let rec pretty_print : Format.formatter -> term -> unit = fun fmt t ->
  match t with
  | Lit i -> Format.fprintf fmt "%i" i
  | Var s -> Format.fprintf fmt "%s" s
  | Any -> Format.fprintf fmt "_"
  | BinOp (t1, op, t2) ->
      Format.fprintf fmt "(%a %s %a)" pretty_print t1 op pretty_print t2
  | MetaVar s -> Format.fprintf fmt "%s" s

let pretty_print_context : Format.formatter -> c_pattern -> unit = fun fmt c_p ->
  match c_p with
  | Term pat -> pretty_print fmt pat
  | InTerm (v, pat) ->
      Format.fprintf fmt "%a in %a" pretty_print v pretty_print pat

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

(* Minimal helpers, for the option pattern matching.
 * TODO: make them less minimal, maybe.
 * *)
let is_none = fun ob ->
  match ob with
  | Some x -> true
  | None -> false

let get = fun ob ->
  match ob with
  | Some x -> x
  | None -> assert false

(* Contextual Pattern Selection.
 * Given a term and a contextual pattern find all occurences of the subterm
 * that would be rewritten in this case.
 * *)

(*
 * This method is given a metavariable and a term containing that metavariable
 * - By assumption the term contains that metavariable exactly once -
 * it returns a term in which
 * *)

let rec context_match : term -> c_pattern -> bool = fun t p ->
  match (t, p) with
  | (Any, _) -> invalid_arg "Invalid term, contains wildcard."
  | (_, Term pat) -> term_match t pat
  | (Lit i, InTerm _) -> false
  | (Var x, InTerm _) -> false
  | (_, InTerm (MetaVar x, MetaVar y)) -> x = y
  | (BinOp(t1, op, t2), InTerm (MetaVar x, BinOp(p1, p_op, p2))) ->
      p_op = op && context_match t1 (InTerm (MetaVar x, p1))
                && context_match t2 (InTerm (MetaVar x, p2))
  | _  -> false

let find_context : term -> c_pattern -> term = fun t c_pat ->
  let rec context_aux : term -> term option = fun cur ->
    if context_match cur c_pat then Some cur else match cur with
    | BinOp (t1, op, t2) ->
        let t1' = context_aux t1 and t2' = context_aux t2 in
        if  is_none t1' then t2' else t1'
    | _ -> None
  in get (context_aux t)

let rec subterm_select : term -> c_pattern -> term list = fun t c_pat ->
  match (t, c_pat) with
  | (t, Term pat) -> matching_list t pat
  | (t, InTerm (MetaVar x, pat)) -> matching_list (find_context t c_pat) t
  | _ -> invalid_arg "Contextual Pattern not of the right form"


