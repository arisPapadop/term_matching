type term =
  | Lit     of int
  | Var     of string
  | BinOp   of term * string * term
  | MetaVar of string
  | Any

type pattern = term

type c_pattern =
  | Term     of pattern
  | InTerm   of term * term (* Single level of depth. *)
  | InInTerm of term * term * term

let rec pretty_print : Format.formatter -> term -> unit = fun fmt t ->
  match t with
  | Lit i -> Format.fprintf fmt "%i" i
  | Var s -> Format.fprintf fmt "%s" s
  | Any -> Format.fprintf fmt "_"
  | BinOp (t1, op, t2) ->
      Format.fprintf fmt "(%a %s %a)" pretty_print t1 op pretty_print t2
  | MetaVar s -> Format.fprintf fmt "%s" s

let pretty_print_context : Format.formatter -> c_pattern -> unit =
  fun fmt c_p ->
  match c_p with
  | Term pat -> pretty_print fmt pat
  | InTerm (v, pat) ->
      Format.fprintf fmt "%a in %a" pretty_print v pretty_print pat
  | InInTerm (v, pat1, pat2) ->
      Format.fprintf fmt "%a in %a in %a"
        pretty_print v pretty_print pat1 pretty_print pat2

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
  | Some x -> false
  | None -> true

let get = fun ob ->
  match ob with
  | Some x -> x
  | None -> raise (Not_found)

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
  | (Lit i, InTerm (_, Lit j)) -> i = j
  | (Var x, InTerm (_, Var y)) -> x = y
  | (_, InTerm (_, Any)) -> true
  | (_, InTerm (MetaVar y, MetaVar x)) -> x = y
  | (BinOp(t1, op, t2), InTerm (MetaVar x, BinOp(p1, p_op, p2))) ->
      p_op = op && context_match t1 (InTerm (MetaVar x, p1))
                && context_match t2 (InTerm (MetaVar x, p2))
  | _  -> false

let rec find_bound_subterm : term -> c_pattern -> term option = fun t p ->
  match (t, p) with
  | (_, Term pat) -> Some (List.hd (matching_list t pat)) (* TODO: Make better. *)
  | (_, InTerm (MetaVar x, MetaVar y)) -> if x = y then Some t else None
  | (BinOp(t1, op, t2), InTerm (MetaVar x, BinOp(p1, p_op, p2))) ->
      if p_op = op then
        match find_bound_subterm t1 (InTerm (MetaVar x, p1)) with
        | None -> find_bound_subterm t2 (InTerm (MetaVar x, p2))
        | Some t -> Some t
      else None
  | _  -> None

let find_context : term -> c_pattern -> term = fun t c_pat ->
  let rec find_context_aux : term -> term option = fun cur ->
    if context_match cur c_pat then find_bound_subterm cur c_pat
    else match cur with
    | BinOp (t1, op, t2) -> let t1' = find_context_aux t1 in
        if is_none t1' then find_context_aux t2 else t1'
    | _ -> None
  in get (find_context_aux t)

let rec subterm_select : term -> c_pattern -> term list = fun t c_pat ->
  match (t, c_pat) with
  | (t, Term pat) -> matching_list t pat
  | (t, InTerm (MetaVar _, _)) -> matching_list t (find_context t c_pat)
  | (t, InInTerm (pat1, MetaVar x, pat2)) ->
      matching_list (find_context t (InTerm (MetaVar x, pat2))) pat1
  | _ -> invalid_arg "Contextual Pattern not of the right form"


