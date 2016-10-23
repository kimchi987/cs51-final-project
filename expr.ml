
(** Abstract syntax of MiniML expressions *)

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of varid * expr                 (* unary operators *)
  | Binop of varid * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;

(** Sets of varids *)
module SS = Set.Make(struct
		      type t = varid
		      let compare = String.compare
		    end);;

type varidset = SS.t ;;

(** Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(** Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;

(** Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  (* only free var is the var itself *)
  | Var v -> SS.singleton v
  (* there are no free vars here *)
  | Num _ | Bool _ | Raise | Unassigned -> SS.empty
  (* the union of the free vars in all the expression(s) *)
  | Unop (v, e) -> free_vars e
  | Binop (v, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) ->
    SS.union (SS.union (free_vars e1) (free_vars e2)) (free_vars e3)
  (* free vars in body minus the argument *)
  | Fun (v, e) -> SS.diff (free_vars e) (SS.singleton v)
  (* free vars in (body - v) and free vars in definition *)
  | Let (v, e1, e2) ->
    SS.union (free_vars e1) (SS.diff (free_vars e2) (SS.singleton v))
  (* free vars in defintion and body both minus v *)
  | Letrec (v, e1, e2) ->
    SS.diff (SS.union (free_vars e1) (free_vars e2)) (SS.singleton v)
  (* free vars in both expressions *)
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
;;

(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let new_varname : unit -> varid =
  let counter = ref 0 in
  fun () ->
    let var = "var" ^ (string_of_int !counter) in
    incr counter; var
;;

(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name: varid) (repl: expr) (exp: expr) : expr =
  match exp with
  (* substitute if v is the var_name you're looking for *)
  | Var v -> if v = var_name then repl else exp
  (* no subbing required here *)
  | Num _ | Bool _ | Raise | Unassigned -> exp
  (* sub into all expression(s) *)
  | Unop (v, e) -> Unop (v, subst var_name repl e)
  | Binop (v, e1, e2) ->
    Binop (v, subst var_name repl e1, subst var_name repl e2)
  | Conditional (e1, e2, e3) -> Conditional (subst var_name repl e1,
    subst var_name repl e2, subst var_name repl e3)
  | Fun (v, e) ->
    if v = var_name then
      exp
    else
      Fun (v, subst var_name repl e)
  | Let (v, e1, e2) ->
    (* if v = var_name, then ocurrances of v in body are bound by v *)
    if v = var_name then
      Let (v, subst var_name repl e1, e2)
    else
      Let (v, subst var_name repl e1, subst var_name repl e2)
  | Letrec (v, e1, e2) ->
    (* if v = var_name, then ocurrances of v in body and def are bound by v *)
    if v = var_name then
      exp
    else
      Letrec (v, subst var_name repl e1, subst var_name repl e2)
  (* sub into all exp(s) *)
  | App (e1, e2) -> App (subst var_name repl e1, subst var_name repl e2)
;;

(* Returns the abstract representation of expr *)
let rec exp_to_abstract (exp: expr) : string =
  let open Printf in
    match exp with
    | Var v -> sprintf "Var(%s)" v
    | Num i -> sprintf "Num(%d)" i
    | Bool b -> "Bool(" ^ (string_of_bool b) ^ ")"
    | Unop (v, e) -> sprintf "Unop(%s, %s)" v (exp_to_abstract e)
    | Binop (v, e1, e2) ->
      sprintf "Binop(%s, %s, %s)" v (exp_to_abstract e1) (exp_to_abstract e2)
    | Conditional (e1, e2, e3) ->
      sprintf "Conditional(%s, %s, %s)" (exp_to_abstract e1)
        (exp_to_abstract e2) (exp_to_abstract e3)
    | Fun (v, e) -> sprintf "Fun(%s, %s)" v (exp_to_abstract e)
    | Let (v, e1, e2) ->
      sprintf "Let(%s, %s, %s)" v (exp_to_abstract e1) (exp_to_abstract e2)
    | Letrec (v, e1, e2) ->
      sprintf "Letrec(%s, %s, %s)" v (exp_to_abstract e1) (exp_to_abstract e2)
    | Raise -> "Raise"
    | Unassigned -> "Unassigned"
    | App (e1, e2) ->
      sprintf "App(%s, %s)" (exp_to_abstract e1) (exp_to_abstract e2)
;;

(* Returns a concrete representation of the expr *)
let rec exp_to_concrete (exp: expr) : string =
  let open Printf in
    match exp with
    | Var v -> v
    | Num i -> sprintf "%d" i
    | Bool b -> string_of_bool b
    | Unop (v, e) -> sprintf "%s(%s)" v (exp_to_concrete e)
    | Binop (v, e1, e2) ->
      sprintf "(%s) %s (%s)" (exp_to_concrete e1) v (exp_to_concrete e2)
    | Conditional (e1, e2, e3) ->
      sprintf "if %s then %s else %s" (exp_to_concrete e1)
        (exp_to_concrete e2) (exp_to_concrete e3)
    | Fun (v, e) -> sprintf "fun %s -> %s" v (exp_to_concrete e)
    | Let (v, e1, e2) ->
      sprintf "let %s = %s in %s" v (exp_to_concrete e1) (exp_to_concrete e2)
    | Letrec (v, e1, e2) ->
      sprintf "let rec %s = %s in %s" v (exp_to_concrete e1)
        (exp_to_concrete e2)
    | Raise -> "Raise"
    | Unassigned -> "Unassigned"
    | App (e1, e2) ->
      sprintf "(%s) (%s)" (exp_to_concrete e1) (exp_to_concrete e2)
;;

(** Returns a string representation of the expr
    made obsolete by exp_to_abstract and exp_to_concrete*)
let exp_to_string = exp_to_abstract ;;
