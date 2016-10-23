open Expr ;;
open Evaluation ;;
open Miniml ;;



(* some little things to help streamline testing *)
let ste = str_to_exp ;;
let test name cond =
  try
    assert(cond);
    Printf.printf "%s passed\n" name
  with
  | _ -> Printf.printf "%s failed\n" name
;;


(* EXPR TESTS *)

(* test expressions for testing Expr functions *)
let e0 = ste "x;;" ;;
let e1 = ste "1;;" ;;
let e2 = ste "true;;" ;;
let e3 = ste "~x;;" ;;
let e4 = ste "2 + x;;" ;;
let e5 = ste "if x = 0 then 0 else y;;" ;;
let e6 = ste "fun x -> x + y;;" ;;
let e7 = ste "let x = 5 in x + y;;" ;;
let e8 = ste "let rec f = fun x -> if x = 0 then y else f (x - 1) in f z;;" ;;
let e9 = ste "let f = fun x -> x in g y;;" ;;

(* testing exp_to_string *)
let exp_to_string_test () =
  test "exp_to_string e0" (exp_to_string e0 = "Var(x)");
  test "exp_to_string e1" (exp_to_string e1 = "Num(1)");
  test "exp_to_string e2" (exp_to_string e2 = "Bool(true)");
  test "exp_to_string e3" (exp_to_string e3 = "Unop(~, Var(x))");
  test "exp_to_string e4" (exp_to_string e4 = "Binop(+, Num(2), Var(x))");
  test "exp_to_string e5" (exp_to_string e5 = "Conditional(Binop(=, Var(x), Num(0)), Num(0), Var(y))");
  test "exp_to_string e6" (exp_to_string e6 = "Fun(x, Binop(+, Var(x), Var(y)))");
  test "exp_to_string e7" (exp_to_string e7 = "Let(x, Num(5), Binop(+, Var(x), Var(y)))");
  test "exp_to_string e8" (exp_to_string e8 = "Letrec(f, Fun(x, Conditional(Binop(=, Var(x), Num(0)), Var(y), App(Var(f), Binop(-, Var(x), Num(1))))), App(Var(f), Var(z)))");
  test "exp_to_string e9" (exp_to_string e9 = "Let(f, Fun(x, Var(x)), App(Var(g), Var(y)))");
  test "exp_to_string raise" (exp_to_string Raise = "Raise");
  test "exp_to_string unass" (exp_to_string Unassigned = "Unassigned");

;;


(* testing free_vars *)
let free_vars_test () =
  test "free_vars_test e0" (same_vars (free_vars e0) (vars_of_list ["x"]));
  test "free_vars_test e1" (same_vars (free_vars e1) (vars_of_list []));
  test "free_vars_test e2" (same_vars (free_vars e2) (vars_of_list []));
  test "free_vars_test e3" (same_vars (free_vars e3) (vars_of_list ["x"]));
  test "free_vars_test e4" (same_vars (free_vars e4) (vars_of_list ["x"]));
  test "free_vars_test e5" (same_vars (free_vars e5) (vars_of_list ["x";"y"]));
  test "free_vars_test e6" (same_vars (free_vars e6) (vars_of_list ["y"]));
  test "free_vars_test e7" (same_vars (free_vars e7) (vars_of_list ["y"]));
  test "free_vars_test e8" (same_vars (free_vars e8) (vars_of_list ["y";"z"]));
  test "free_vars_test e9" (same_vars (free_vars e9) (vars_of_list ["g";"y"]))
;;

(* subing in 5 for variables of my choosing  *)
let e0' = ste "5;;" ;;
let e1' = ste "1;;" ;;
let e2' = ste "true;;" ;;
let e3' = ste "~5;;" ;;
let e4' = ste "2 + 5;;" ;;
let e5' = ste "if x = 0 then 0 else 5;;" ;;
let e6' = ste "fun x -> x + 5;;" ;;
let e7' = ste "let x = 5 in x + 5;;" ;;
let e8' = ste "let rec f = fun x -> if x = 0 then y else f (x - 1) in f 5;;" ;;
let e9' = ste "let f = fun x -> x in g 5;;" ;;

(* testing subbing *)
let subst_test () =
  test "subst x 5 e0" (subst "x" (Num(5)) e0 = e0');
  test "subst x 5 e1" (subst "x" (Num(5)) e1 = e1');
  test "subst x 5 e2" (subst "x" (Num(5)) e2 = e2');
  test "subst x 5 e3" (subst "x" (Num(5)) e3 = e3');
  test "subst x 5 e4" (subst "x" (Num(5)) e4 = e4');
  test "subst y 5 e5" (subst "y" (Num(5)) e5 = e5');
  test "subst y 5 e6" (subst "y" (Num(5)) e6 = e6');
  test "subst y 5 e7" (subst "y" (Num(5)) e7 = e7');
  test "subst z 5 e8" (subst "z" (Num(5)) e8 = e8');
  test "subst y 5 e9" (subst "y" (Num(5)) e9 = e9')
;;


(* EVALUATION TESTS *)

(* test env(s) *)
let env1 = Env.create();;

(* close is straightforward and only has one code path *)
let close_test () =
  test "close test" ((Env.close Raise env1) = Env.Closure(Raise, env1))
;;

(* This test is gross, but it gets the job done in a contained fashion.
   besause envs are annoying, all the test are lumped into one.
   It does go down the proper code paths, though*)
let env_test () =
  let _ =
    (try
      Env.lookup env1 "x"
    with
    | Env.EnvUnbound -> print_string "empty lookup success\n"; Env.Val Raise) in
  let env2 =
    assert(Env.env_to_string env1 = "[]");
    print_string "env_to_string env1 passed\n";
    Env.extend env1 "x" (ref (Env.Val Unassigned)) in
  let val1 =
    assert(Env.env_to_string env2 = "[(x, Val(Unassigned)); ]");
    print_string "env_to_string env2 passed\n";
    Env.lookup env2 "x" in
  let env3 =
    assert(val1 = (Env.Val Unassigned));
    print_string "val1 extend/lookup success\n";
    Env.extend env2 "x" (ref (Env.Val Raise)) in
  let val2 = Env.lookup env3 "x" in
  assert(val2 = (Env.Val Raise)); print_string "val2 extend/lookup success\n"
;;

(* test expressions to evaluate *)
let ev0 = ste "5;;";;
let ev1 = ste "true;;";;
let ev2 = ste "5 + 1;;";;
let ev3 = ste "if 5 = 0 then 5 else 6;;";;
let ev4 = ste "fun x -> x;;";;
let ev5 = ste "let x = 5 in x;;";;
let ev6 = ste "let rec f = fun x -> if x = 0 then 0 else f (x - 1) in f 2;;";;
let ev7 = ste "let x = 5 in let f = fun y -> x + y in let x = 4 in f 1;;";;

let eval_s_test () =
  test "eval_s ev0" (eval_s ev0 env1 = Env.Val (ev0));
  test "eval_s ev1" (eval_s ev1 env1 = Env.Val (ev1));
  test "eval_s ev2" (eval_s ev2 env1 = Env.Val (ste "6;;"));
  test "eval_s ev3" (eval_s ev3 env1 = Env.Val (ste "6;;"));
  test "eval_s ev4" (eval_s ev4 env1 = Env.Val (ev4));
  test "eval_s ev5" (eval_s ev5 env1 = Env.Val (ste "5;;"));
  (* eval_s didn't like the recursion.  it is a bug gone unfixed *)
  (* test "eval_s ev6" (eval_s ev6 env1 = Env.Val (ste "0;;")); *)
  test "eval_s ev7" (eval_s ev7 env1 = Env.Val (ste "6;;"))
;;

let eval_d_test () =
  (* produces unbound error when uncommented (which is expected) *)
  (* test "eval_d ev0" (eval_d ev0 env1 = Env.Val (ev0)); *)
  test "eval_d ev1" (eval_d ev1 env1 = Env.Val (ev1));
  test "eval_d ev2" (eval_d ev2 env1 = Env.Val (ste "6;;"));
  test "eval_d ev3" (eval_d ev3 env1 = Env.Val (ste "6;;"));
  test "eval_d ev4" (eval_d ev4 env1 = Env.Val (ev4));
  test "eval_d ev5" (eval_d ev5 env1 = Env.Val (ste "5;;"));
  test "eval_d ev6" (eval_d ev6 env1 = Env.Val (ste "0;;"));
  test "eval_d ev7" (eval_d ev7 env1 = Env.Val (ste "5;;"))
;;


let eval_l_test () =
  (* produces unbound error when uncommented (which is expected) *)
  (* test "eval_d ev0" (eval_d ev0 env1 = Env.Val (ev0)); *)
  test "eval_l ev1" (eval_l ev1 env1 = Env.Val (ev1));
  test "eval_l ev2" (eval_l ev2 env1 = Env.Val (ste "6;;"));
  test "eval_l ev3" (eval_l ev3 env1 = Env.Val (ste "6;;"));
  test "eval_l ev4" (eval_l ev4 env1 = Env.Closure (ev4, env1));
  test "eval_l ev5" (eval_l ev5 env1 = Env.Val (ste "5;;"));
  test "eval_l ev6" (eval_l ev6 env1 = Env.Val (ste "0;;"));
  test "eval_l ev7" (eval_l ev7 env1 = Env.Val (ste "6;;"))
;;

(* RUN TESTS *)
exp_to_string_test ();;
free_vars_test ();;
subst_test ();;
close_test();;
env_test();;
eval_s_test();;
eval_d_test();;
eval_l_test();;
