(* MiniML: a subset of ML
   Read-eval-print loop
   Stuart M. Shieber
   November 19, 2015
 *)

(* debug flag to enable printing of abstract structure.
  ensures proper parsing *)
let debug = false ;;

open Printf ;;

(* str_to_exp: string -> expr
   Returns the expression specified by the string using the Miniml
   parser. *)
let str_to_exp (str: string) : Expr.expr =
  let lexbuf = Lexing.from_string str in
  let exp = Miniml_parse.input Miniml_lex.token lexbuf in
  exp
;;

(* Read-eval-print loop for MiniML *)

let repl () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  (* set up the initial environment *)
  let env = Evaluation.Env.create () in
  printf "Entering %s...\n" Sys.argv.(0);
  flush stdout;
  while true do
    (try
        printf "<== ";
	flush stdout;
	(* read and parse an expression from the input *)
        let exp = Miniml_parse.input Miniml_lex.token lexbuf in
        (* optionally print Abstract tree *)
        let _ =
          if debug then printf "<== %s\n" (Expr.exp_to_abstract exp) else () in
        (* evaluate it *)
        let res = Evaluation.evaluate exp env in
        (* print the result *)
	match res with
	| Evaluation.Env.Val res ->  printf "==> %s\n" (Expr.exp_to_concrete res)
	| Evaluation.Env.Closure(_,_) -> raise (Evaluation.EvalError "can't happen")
      with
      | Parsing.Parse_error -> printf "xx> parse error\n"
      | End_of_file -> printf "Goodbye.\n"; exit 0
      | Evaluation.EvalError s -> printf "xx> evaluation error: %s\n" s
      | Evaluation.EvalException -> printf "xx> exception raised\n"
      | Evaluation.Env.EnvUnbound -> printf "xx> unbound value in env\n"
    );
    flush stdout
  done
;;

(* Run repl if called from command line *)

try
  let _ = Str.search_forward (Str.regexp "miniml\\.\\(byte\\|native\\)") (Sys.argv.(0)) 0 in
  repl ()
with Not_found -> () ;;
