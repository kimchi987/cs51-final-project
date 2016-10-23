(** A mini-ML
    @author Stuart M. Shieber

    This module implements a small untyped ML-like language under
    various operational semantics.
 *)

open Expr ;;

(* Exception for evaluator runtime generated by a runtime error *)
exception EvalError of string ;;
(* Exception for evaluator runtime generated by an explicit "raise" construct *)
exception EvalException ;;

module type Env_type = sig
    type env
    type value =
       | Val of expr
       | Closure of (expr * env)
    exception EnvUnbound
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : Env_type =
  struct

    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    exception EnvUnbound

    (* Creates an empty environment *)
    let create () : env = [] ;;

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp: expr) (env: env) : value =
      Closure (exp, env) ;;

    (* Looks up the value of a variable in the environment *)
    let lookup (env: env) (varname: varid) : value =
      try
        !(snd (List.find (fun x -> fst x = varname) env))
      with
      | Not_found -> raise EnvUnbound;;

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let rec extend (env: env) (varname: varid) (loc: value ref) : env =
      match env with
      | [] -> (varname, loc) :: []
      | (varid, vref) :: tl ->
        if varid = varname then
          (varid, loc) :: tl
        else
          (varid, vref) :: (extend tl varname loc) ;;

    (* Returns a printable string representation of an environment *)
    let rec env_to_string (env: env) : string =
      let rec env_to_string' env =
        match env with
        | [] -> "]"
        | (vid, value_ref)::tl ->
          (Printf.sprintf "(%s, %s); " vid (value_to_string !value_ref))
            ^ env_to_string' tl
      in "[" ^ env_to_string' env
    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    and value_to_string ?(printenvp : bool = true) (v: value) : string =
      match v with
      | Val exp -> "Val(" ^ exp_to_abstract exp ^ ")"
      | Closure (exp, env) ->
        if printenvp then
          "Closure(" ^ exp_to_abstract exp ^ ", " ^ env_to_string env ^ ")"
        else
          "Closure(" ^ exp_to_abstract exp ^ ", <env>)";;
  end
;;

(* The evaluation function: Returns the result of type `value` of
   evaluating the expression `exp` in the environment `env`. In this
   initial implementation, we just convert the expression unchanged to
   a value and return it. *)


let eval_t (exp : expr) (_env : Env.env) : Env.value = Env.Val exp ;;

let eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec eval_s' (exp : expr) : expr =
    match exp with
    (* returns themselves.  vars should not exist on their own *)
    | Var v -> raise (EvalError ("Unbound value " ^ v))
    | Num i -> exp
    | Bool b -> exp
    | Unop (v, e) ->
      (* makes sure v is ~ and e evals to a num *)
      (match v with
      | "~" ->
        (match eval_s' e with
        | Num i -> Num (i * -1)
        | _ -> raise (EvalError "integer expected"))
      | _ -> raise (EvalError "invalid operator"))
    | Binop (v, e1, e2) ->
      (* both sides must be nums except in = where it could be two bools *)
      (match eval_s' e1, eval_s' e2 with
      | Num a, Num b ->
        (* matches with and performs proper operation *)
        (match v with
        | "+" -> Num (a + b)
        | "-" -> Num (a - b)
        | "*" -> Num (a * b)
        | "=" -> Bool (a = b)
        | "<" -> Bool (a < b)
        | _ -> raise (EvalError "invalid operator"))
      | Bool a, Bool b ->
        (match v with
        | "=" -> Bool (a = b)
        | _ -> raise (EvalError "invalid operator"))
      | _ -> raise (EvalError "invalid type(s)"))
    | Conditional (e1, e2, e3) ->
      (* e1 must be a bool.  if true eval e1 else eval e2 *)
      (match eval_s' e1 with
      | Bool b -> if b then eval_s' e2 else eval_s' e3
      | _ -> raise (EvalError "boolean expression expected"))
    (* functions eval to themselves *)
    | Fun (v, e) -> exp
    | Let (v, e1, e2) ->
      (* eval e1 and sub it in for all ocurrences of v in e2 *)
      let e1' = eval_s' e1 in
        eval_s' (subst v e1' e2)
    | Letrec (v, e1, e2) ->
      (* done according to lecture slides on subbstitution model *)
      let e1' = eval_s' (subst v (Letrec (v, e1, Var v)) e1) in
        eval_s' (subst v e1' e2)
    | Raise -> raise EvalException
    | Unassigned -> raise (EvalError "Unassigned")
    | App (e1, e2) ->
      (* done according to lecture slides on substitution function evaluation *)
      (match eval_s' e1 with
      | Fun (v, e) ->
        let e2' = eval_s' e2 in eval_s' (subst v e2' e)
      | _ -> raise (EvalError "function expected"))
  in Env.Val (eval_s' exp)
;;

let eval_d (exp : expr) (env : Env.env) : Env.value =
  let rec eval_d' (exp : expr) (env : Env.env) : expr =
    match exp with
    (* if in env, return the expression else unbound and exception raised *)
    | Var v ->
      (match Env.lookup env v with
      | Env.Val expr -> expr
      | _ -> raise (EvalError ("unbound variable " ^ v)))
    (* num and bool evaluate to themselves *)
    | Num i -> exp
    | Bool b -> exp
    | Unop (v, e) ->
      (* makes sure v is ~ and e evals to a num *)
      (match v with
      | "~" ->
        (match eval_d' e env with
        | Num i -> Num (i * -1)
        | _ -> raise (EvalError "integer expected"))
      | _ -> raise (EvalError "invalid operator"))
    | Binop (v, e1, e2) ->
      (* both sides must be nums except in = where it could be two bools *)
      (match eval_d' e1 env, eval_d' e2 env with
      | Num a, Num b ->
        (* matches with and performs proper operation *)
        (match v with
        | "+" -> Num (a + b)
        | "-" -> Num (a - b)
        | "*" -> Num (a * b)
        | "=" -> Bool (a = b)
        | "<" -> Bool (a < b)
        | _ -> raise (EvalError "invalid operator"))
      | Bool a, Bool b ->
        (match v with
        | "=" -> Bool (a = b)
        | _ -> raise (EvalError "invalid operator"))
      | _ -> raise (EvalError "invalid type(s)"))
    | Conditional (e1, e2, e3) ->
      (* e1 must be a bool.  if true eval e1 else eval e2 *)
      (match eval_d' e1 env with
      | Bool b -> if b then eval_d' e2 env else eval_d' e3 env
      | _ -> raise (EvalError "boolean expression expected"))
    (* functions still evaluate to themselves *)
    | Fun (v, e) -> exp
    | Let (v, e1, e2) ->
      (* extend env to include v as evaluated e1 then eval e2 in this new env *)
      let env' = Env.extend env v (ref (Env.Val (eval_d' e1 env))) in
      eval_d' e2 env'
    | Letrec (v, e1, e2) ->
      (* follows spec. make new environment with unassigned v, eval definition
         in this new environment, extend environment again to include definition
         and finally evaluate the body in the new new environment*)
      let env' = Env.extend env v (ref (Env.Val Unassigned)) in
      let e1' = eval_d' e1 env' in
      let env'' = Env.extend env' v (ref (Env.Val e1')) in
      eval_d' e2 env''
    | Raise -> raise EvalException
    | Unassigned -> raise (Env.EnvUnbound)
    | App (e1, e2) ->
      (* done according to spec on dynamical function evaluation *)
      (match eval_d' e1 env with
      | Fun (v, e) ->
        let e2' = eval_d' e2 env in
        eval_d' e (Env.extend env v (ref (Env.Val e2')))
      | _ -> raise (EvalError "function expected"))
  in Env.Val (eval_d' exp env)
;;

(* eval_d with modified logic for functions and applicaitons *)
let rec eval_l (exp : expr) (env : Env.env) : Env.value =
  match exp with
  (* var lookup now just returns whatever lookup returns*)
  | Var v -> Env.lookup env v
  (* these evaluate to the value of themselves *)
  | Num _ | Bool _ | Raise | Unassigned -> Env.Val exp
  | Unop (v, e) ->
    (* makes sure v is ~ and e evals to a num *)
    (match v with
    | "~" ->
      (match eval_l e env with
      | Env.Val (Num i) -> Env.Val (Num (i * -1))
      | _ -> raise (EvalError "integer expected"))
    | _ -> raise (EvalError "invalid operator"))
  | Binop (v, e1, e2) ->
    (* both sides must be nums except in = where it could be two bools *)
    (match eval_l e1 env, eval_l e2 env with
    | Env.Val (Num a), Env.Val (Num b) ->
      (* matches with and performs proper operation *)
      (match v with
      | "+" -> Env.Val (Num (a + b))
      | "-" -> Env.Val (Num (a - b))
      | "*" -> Env.Val (Num (a * b))
      | "=" -> Env.Val (Bool (a = b))
      | "<" -> Env.Val (Bool (a < b))
      | _ -> raise (EvalError "invalid operator"))
    | Env.Val (Bool a), Env.Val (Bool b) ->
      (match v with
      | "=" -> Env.Val (Bool (a = b))
      | _ -> raise (EvalError "invalid operator"))
    | _ -> raise (EvalError "invalid type(s)"))
  | Conditional (e1, e2, e3) ->
    (* e1 must be a bool.  if true eval e1 else eval e2 *)
    (match eval_l e1 env with
    | Env.Val (Bool b) -> if b then eval_l e2 env else eval_l e3 env
    | _ -> raise (EvalError "boolean expression expected"))
  | Fun (v, e) -> Env.close exp env
  | Let (v, e1, e2) ->
    (* extend env to include v as evaluated e1 then eval e2 in this new env *)
    let env' = Env.extend env v (ref (eval_l e1 env)) in
    eval_l e2 env'
  | Letrec (v, e1, e2) ->
    (* follows spec. make new environment with unassigned v, eval definition
       in this new environment, extend environment again to include definition
       and finally evaluate the body in the new new environment*)
    let f_ref = ref (Env.Val Unassigned) in
    let env' = Env.extend env v f_ref in
    let e1' = eval_l e1 env' in
    let env'' = Env.extend env' v (ref e1') in
    f_ref := e1';
    eval_l e2 env''
  | App (e1, e2) ->
    (* done according to spec on dynamical function evaluation *)
    (match eval_l e1 env with
    | Env.Closure (Fun (v, e), cenv) ->
        let e2' = eval_l e2 env in
        eval_l e (Env.extend cenv v (ref e2'))
    | _ -> raise (EvalError "function expected"))
;;

(* evaluate function that is called in miniml.ml *)
let evaluate = eval_l ;;
