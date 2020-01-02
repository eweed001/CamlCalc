open Ast
open Stdlib
open Lexing
open Printf
open Imports

module type Main_Sig = sig
  val interp : string -> env -> (string * env)
  val run : unit -> unit
  val initial_env : env
end

module Main = struct

  (** Exceptions for errors in parsing phrases input by the user *)
  exception SyntaxError of string
  exception UnexpectedError of string

  (** [parse_error lexbuf] is the error to raise when parser/lexeing raises
      a parsing or lexing error *)
  let parse_error lexbuf = raise (SyntaxError "Syntax error, please try again")

  (** [unexp_error lexbuf] is the error to raise when parser/lexeing fails *)
  let unexp_err lexbuf = raise
      (UnexpectedError "Unexepcted error, please try again")

  (** [parse parser_start s] parses [s] as a phrase using [parser_start] *)
  let parse parser_start s =
    let lexbuf = from_string s in
    try parser_start Lexer.read lexbuf with
    | Parser.Error | Lexer.Syntax_error -> parse_error lexbuf
    | Failure s -> unexp_err s

  (** [parse_phrase s] parses [s] as a phrase *)
  let parse_phrase = parse Parser.prog

  (** [return_head lst] is the first element of [lst] *)
  let return_head = function
    |h::t -> h
    | _ -> failwith "This case is not reached"


  (** [values_to_float lst] is the float list representation of [lst]
      Requires: all elements in [lst] are of type VFloat *)
  let rec values_to_floats = function
    |[] -> []
    |VFloat f :: t -> f :: values_to_floats t
    | _ -> failwith "not right type of argument"

  (** [string_of_floatlst lst] is the string representation of [lst]
      Requires: all elements of [lst] are floats *)
  let rec string_of_floatlst lst =
    match lst with
    |[] -> ""
    |h :: t -> if t = [] then
        string_of_float h ^"" ^ string_of_floatlst t else
        string_of_float h ^", " ^ string_of_floatlst t

  (** [matrix_to_string_helper lst fst_element] is the string representation
      of each row of the matrix [lst] *)
  let rec matrix_to_string_helper lsts fst_element=
    match lsts with
    |[] -> ""
    |h::t -> if (h == fst_element) then
        "|"^ string_of_floatlst h ^"|" ^
        matrix_to_string_helper t fst_element else
        "\n |"^ string_of_floatlst h ^"|" ^
        matrix_to_string_helper t fst_element

  (** [matrix_to_string m] is the string representation of the matrix [lst] *)
  let matrix_to_string m =
    let lst = Array.to_list (Array.map Array.to_list m) in
    let s = "["^ (matrix_to_string_helper lst (List.nth lst 0))^ "]"in s

  (** [string_of_expr e] converts [e] to a string.
      Requires: [e] is an expression. *)
  let string_of_expr e =
    match e with
    |Float i -> string_of_float i
    |Boolean b -> string_of_bool b
    |String s -> s
    |If _ -> "if expression"
    |Let _ -> "let"
    |Fun (s, e)-> "fun"
    |FunApp (s, e) -> "fun app"
    |Sequence (e1,e2) -> "sequence of expressions"
    |DSequence (d,e) -> "Definition Sequence"
    |Var x -> x
    |Binop _ -> "binop"
    |Unop _ -> "unop"
    |Arr _ -> "arr"


  (** [string_of_value v] is the string representation of value [v] *)
  let string_of_value  = function
    |VBool b -> string_of_bool b
    |VFloat s -> string_of_float s
    |VString s -> "\"" ^ String.escaped s ^ "\""
    |VBinop b -> "\"" ^ String.escaped b ^ "\""
    |VId x -> "\"" ^ String.escaped x ^ "\""
    |Closure (x, e, env) -> "<closure>"
    |Extern e -> "<extern>"
    |VRow r ->
      let s = Array.fold_left (fun acc s -> acc^ " " ^ 
                                            (string_of_float s)) "" r
      in "["^s^"]"
    |VFloatList _ -> "float list values"
    |VMatrix m -> matrix_to_string m

  (** [unwrap v] is the float extracted from value [v] 
        Requires: [v] has type VFloat *)
  let unwrap_float v =
    match v with
    | VFloat x -> x
    | _ -> failwith "This cannot occur - arithmetic.ml"

  (** [step env expr] is the [value] where [expr] steps to [value] when 
      evaluated in environment [env] *)
  let rec step (curr_env:env) expr =
    match expr with
    | Float x -> VFloat (x)
    | Var y ->  Env.find y curr_env
    | String s ->  (VString s)
    | Boolean b -> (VBool b)
    | Binop (bop, e1, e2)  ->
      step_bop bop e1 e2 curr_env
    | Let (x, e1, e2) -> eval_let_expr curr_env x e1 e2
    | If (e1, e2, e3) -> eval_if e1 e2 e3 curr_env
    | Fun (s,e) -> Closure (s, e, curr_env)
    | Sequence (e1,e2) -> eval_seq e1 e2 curr_env
    | DSequence (d,e) -> eval_dseq d e curr_env
    | FunApp (e1,e2) -> eval_fun e1 e2 curr_env
    | Unop (unop, e) -> eval_unop unop e curr_env
    | Arr (e) -> fst (eval_row e curr_env)

  (** [eval_unop up e env] is the [value] that [up] [e] evaluates to in 
      environment [env] *)
  and eval_unop uop e env =
    let v = step env e in
    match uop, v with
    |Func_u str, v1 -> (Imports.find_function str) [v1]

  (** [eval_if e1 e2 e3 env] is the [value] that is the result of evaluating
      if [e1] then [e2] else [e3] in environment [env] *)
  and eval_if e1 e2 e3 env =
    let v = step env e1 in
    match v with
    |VBool true ->  step env e2
    |VBool false -> step env e3
    |VFloat f -> if f = 1. then step env e2 else step env e3
    | _ -> failwith "if guard error"

  (** [step_bop bop e1 e2 env] is the [value] of the evaluation of  the 
      primitive operation [v1 bop v2] in environment [env]
      Requires: [v1] and [v2] are both values. *)
  and step_bop bop e1 e2 env =
    let e1' = step env e1 in
    let e2' = step env e2 in
    match bop, e1', e2' with
    | Func str, v1, v2 ->
      (Imports.find_function str) [v1;v2]

  (** [string_of_var e] is the string extracted from [e]
      Requires: [e] is of type [Var] *)
  and string_of_var e = match e with
    | Var e -> e
    |_ -> failwith "not vaild identifier for let statement"

  (** [eval_let_expr env x e1 e2] is the [value] that is the result of 
      evaluating let [x] = [e1] in [e2] in environment [env] *)
  and eval_let_expr env x e1 e2 =
    let v1 = step env e1  in
    let env' = Env.add x v1 env in
    let v = step env' e2 in v

  (** [eval_fun e1 e2 env] is the [value] resulting from evaluating 
      [e1] to a [Closure] or [Extern] and applying that value to [e2] within
      environment [env] *)
  and eval_fun e1 e2 env =
    let v' = step env e1  in
    match v' with
    |Closure( s, e, env') -> begin
        let v2 = eval_id_list e2 env in
        if (List.length s <> List.length v2)
        then
          failwith "wrong number of arguments"
        else
          let base_env = env' in
          let env_for_body = add_bindings s v2 base_env in
          step env_for_body e
      end
    |Extern (ExtFun f) ->
      let v2 = eval_id_list e2 env  in
      (f v2)
    |Extern (GExtFun g) ->
      let v2 = (eval_id_list e2 env) in
      step env (g (List.nth v2 0 , List.nth v2 1, List.nth v2 2, env))
    | Extern (MExtFun g) ->
      let v2 = (eval_id_list e2 env) in
      (g v2 env)
    |_-> failwith "function failure"


  (** [eval_id_list e2 env] is the [value] list where each expression in [e2] 
      was stepped within environment [env] t *)
  and eval_id_list e2 env =
    match e2 with
    |[] -> []
    |e :: t -> (step env e ):: (eval_id_list t env)

  (** [add_bindings ids values env] is the [env] with the [values] bound 
      to [ids] *)
  and add_bindings ids values env =
    match (ids, values) with
    | ([], []) -> env
    | (id :: idt, v :: vt) -> let env' =  Env.add id (v) env in
      add_bindings idt vt env'
    | _ -> env

  (** [eval_seq e1 e2 env] is the [value] that [e2] steps to in environment 
      [env] after evaluating [e2]  *)
  and eval_seq e1 e2 curr_env = 
    let _ = step curr_env e1 in 
    step curr_env e2

  (** [eval_dseq d e env] is the [value] that [e] steps to in environment 
        [env'] after evaluating [d] to [(v, env')] *)
  and eval_dseq d e cur_env = 
    let (v , env') = eval_defn cur_env d in 
    step env' e

  (** [eval_defn env e] evaluates [e] to [(v, env')] in environment [env] *)
  and eval_defn env e = 
    match e with
    |DLet (id, e1) -> eval_let_defn env id e1

  (** [eval_row_id_list lst env] is the [value] list representation of [lst] 
      in environment [env] 
      Requires: all elements in [lst] are of type [Var] *)
  and eval_row_id_list lst env =
    match lst with
    |[] -> []
    |h :: t -> let h = Var h in (step env h) :: (eval_row_id_list t env)

  (** [array_getter e] is the float array extracted from [e]
      Requires: [e] is of type [VRow] *)
  and array_getter e =
    match e with
    |VRow e -> e
    |_ -> failwith "wrong type of arguement"

  (** [eval_let_defn env id e] is the [(v,env')] after evaluating [e] to [v] 
      and adding [id] bound to [v] to [env] creating [env'] *)
  and eval_let_defn env1 id e =
    let v = step env1 e  in
    let env' = Env.add id v env1 in
    (v, env')

  (** [float_getter v] returns v1 if [v] is VFloat v1. Fails with "invalid 
      argument" otherwise. *)
  and float_getter v =
    match v with
    |VFloat v -> v
    |_ -> failwith "invalid argument"

  (** [eval_id_lst_flts lst env] is a list of floats by evaluating each element
      in [lst] within the current environment [curr_env] *)
  and eval_id_lst_flts lst env =
    match lst with
    |[] -> []
    |e :: t -> ( float_getter (step env e)) :: (eval_id_lst_flts t env)

  (** [eval_row lst curr_env] evaluates [lst] to a CAMLCALC Row type within the
      current environment [curr_env] *)
  and eval_row lst curr_env =
    let a = eval_id_list lst curr_env in
    let a' = values_to_floats a in
    let array = Array.make (List.length a') 0. in
    let () = for i = 0 to ((List.length a') - 1) do
        array.(i) <- List.nth a' i
      done in
    (VRow array, curr_env)

  (** [eval_matrix lst curr_env] evaluates [lst] to a CAMLCALC Matrix within the
      environment [curr_env] *)
  and eval_matrix (lst : value list) (curr_env : env) =
    let _column_length = match List.hd lst with
      | VRow v -> Array.length v
      | _ -> failwith "eval_matrix failure, cannot be a row"
    in
    let a = 
      List.map 
        (fun v -> 
           match v with
           |VRow f -> f
           | _ -> failwith "eval_matrix failure, cannot be a row")
        lst
      |> Array.of_list
    in
    VMatrix a

  (** [eval_phrase env exp] evaluates [exp] within the environment [env] *)
  let rec eval_phrase env exp =
    match exp with
    |Expr e -> (step env e, env)
    |Defn d -> eval_defn env d


  (** [interp s] interprets [s] by parsing and evaluating it. *)
  let interp (s : string) (curr_env: env) : (string * env) =
    try (
      let expr = s |> parse_phrase |> eval_phrase curr_env in
      let v' = fst expr in
      let env = snd expr in
      let str = string_of_value v' in
      (str, env)
    )
    with
    |SyntaxError s |Failure s -> (s, curr_env)

  (** [days] is the array of possible values of the days of the week *)
  let days = [| "Sunday"; "Monday"; "Tuesday"; "Wednesday";
                "Thursday";"Friday"; "Saturday"|]
  (** [months] is the array of possible values of the months *)
  let months = [| "January"; "February"; "March"; "April"; "May"; "June";
                  "July"; "August"; "September"; "October";
                  "November"; "Dececmber" |]

  (** [time_helper tm] prints out the current time and date when prompted
      by the user  *)
  let time_helper tm =
    let tm' = Unix.localtime tm in
    let () = printf "Date -  %s %s %d, %d \n"
        days.(tm'.tm_wday) months.(tm'.tm_mon) 
        tm'.tm_mday (tm'.tm_year + 1900) in
    let () = printf "Time -  %02d:%02d:%02d"
        tm'.tm_hour tm'.tm_min tm'.tm_sec in ()


  (** [text_file_reader chnl] reads line by line and prints it out from 
        [chnl] until [End_of_file] is reached *)
  let rec text_file_reader chnl =
    match input_line chnl with
    |s -> print_endline s; text_file_reader chnl
    |exception End_of_file -> close_in chnl

  (** [code_file_reader chnl env] reads from [chnl] line by line, interpreting 
      each line in CAMLCALC, printing each line and result. Returns [env'] 
      which is the updated environment after [End_of_file] was reached
       in [chnl] *)
  let rec code_file_reader chnl env =
    match input_line chnl with
    |s -> print_endline s;let r = interp s env in
      let () = print_endline (fst r) in code_file_reader chnl (snd r)
    |exception End_of_file -> close_in chnl; env


  let rec main () curr_env =
    print_string ">";
    match String.trim (String.lowercase_ascii (read_line())) with
    |"quit" -> ()
    |"clear" -> let _ = Unix.system "clear"in main () curr_env
    |"help" -> let chnl = open_in "help.txt" in 
      text_file_reader chnl; main () curr_env
    |"time" -> let tm = Unix.time() in
      time_helper tm;print_endline (""); main () curr_env
    |"monty hall game"-> let () = Monty.start() in main() curr_env
    |"monty hall explanation" -> let chnl = open_in "monty_explain.txt" in
      text_file_reader chnl; main() curr_env
    |"break the code game" -> let () = Breakthecode.start() in main () curr_env
    |"statistics help" -> let chnl = open_in "statistics_help.txt" in 
      text_file_reader chnl; main() curr_env
    |"fibonacci help" -> let chnl = open_in "fibonacci_help.txt" in 
      text_file_reader chnl; main() curr_env
    |"code files help" -> let chnl = open_in "code_files_help.txt" in 
      text_file_reader chnl; main() curr_env
    |"matrix help" -> let chnl = open_in "matrix_help.txt" in 
      text_file_reader chnl; main() curr_env
    |"sets help" -> let chnl = open_in "sets_help.txt" in 
      text_file_reader chnl; main() curr_env
    |"graphing help" -> let chnl = open_in "graphing_help.txt" in 
      text_file_reader chnl; main() curr_env
    |e ->

      try (
        if (((String.length e) > 5) && 
            ((String.sub e ((String.length e) - 4) (4)) = ".txt") ) then
          let chnl = open_in e in 
          let env' =  code_file_reader chnl curr_env in 
          main() env'
        else
          match (interp e curr_env) with
          |exception Not_found -> 
            print_endline "Not a valid command please try again";
            main () curr_env
          |(s, env) -> print_endline s;
            print_endline "";
            main () env)

      with
      |Failure s -> print_endline "Not a valid command please try again"; 
        main () curr_env
      |Invalid_argument e ->
        print_endline "Not a valid command please try again"; main () curr_env


  (** [graph c left_bound right_bound env] is the boolean true, and graphs the
      value [c] on the x,y plane, from [left_bound] to [right_bound], in
      the environment [env]. 
      Graphing applies the closure [c] for each element in
      a set of data points between [left_bound] and [right_bound] with a set
      precision.
      Requires: [c] is a VClosure that contains a function mapping a single 
      float input [x] to a float output. *)
  let graph (c, left_bound, right_bound, env) =
    let (ids, f, env') =
      match c with
      | Closure (ids, f , env') -> (ids, f, env')
      | _ -> failwith "Cannot graph a non-closure main.ml : graph"
    in
    let out_file = "data.dat" in
    let out_handle = open_out out_file in
    let left_int = (left_bound |> unwrap_float |> int_of_float)*100 in
    let right_int = (right_bound |> unwrap_float |> int_of_float)*100 in
    for x=left_int to right_int do
      try
        let y =
          step
            env
            (FunApp (Fun (["x"], f), [Float (Float.of_int (x / 100))]))
        in
        fprintf out_handle "%d %f\n" (x/100) (y |> unwrap_float)
      with
      | e -> ()
    done;
    close_out out_handle;
    ignore (Sys.command "gnuplot -c gnuplot_script.txt");
    if (Sys.command "which open" )= 0 then ignore (Sys.command "open fig.png")
    else if Sys.command "which xdg-open" = 1 then ignore
        (Sys.command "xdg-open fig.png");
    Boolean true

  (** [derivative_helper c x1_val h_val] calculates the derivative of [c] at 
      [x1_val] with an approximation of [h_val]. Returns error if [c] is not a 
      closure *)
  let derivative_helper (c : value) (x1_val : value) (h_val : value) =
    let (ids, expr, env) =
      match c with
      | Closure (ids, expr , env') -> (ids, expr, env')
      | _ -> failwith "Cannot graph a non-closure main.ml : graph"
    in
    let f = (Fun (["x"], expr)) in
    let x1 = x1_val |> unwrap_float in
    let h = h_val |> unwrap_float in
    let y1 = step env (FunApp (f,[Float x1])) |> unwrap_float in
    let x2 = Float.add x1 h in
    let y2 = step env (FunApp (f,[Float x2])) |> unwrap_float in
    (Float.div (Float.sub y2 y1) (h_val |> unwrap_float ))

  (** [derivative v] calculates the derivative of the first element of [v] at
      a point given by the second element of [v] and with an approximation given
      by the third element of [v] 
      Requires: [v] is of length = 3 and is of ordered types function, float, 
      float *)
  let derivative (v : value list) =
    let c = List.nth v 0 in
    let x1_val = List.nth v 1 in
    let h_val = List.nth v 2 in
    VFloat (derivative_helper c x1_val h_val)

  (** [trapezoid c v v1] calculates the trapezoidal integral approximation of
      [c] from [v] to [v1]. *)
  let trapezoid (c : value) (v : float) (v1 : float) =
    let (ids, expr, env) =
      match c with
      | Closure (ids, expr , env') -> (ids, expr, env')
      | _ -> failwith "Cannot graph a non-closure main.ml : graph"
    in
    let f = (Fun (["x"], expr)) in
    let left_height = step env (FunApp (f,[Float v])) |> unwrap_float in
    let right_height = step env (FunApp (f, [Float v1])) |> unwrap_float in
    Float.mul
      (Float.mul 0.5 (Float.add (left_height) (right_height)))
      (Float.sub v1 v)

  (** [integrate_helper f v v1 acc] is the result of integrating f and 
      evaluating it from [v] to [v1]. *)
  let rec integrate_helper
      (f : value) (v : float) (v1 : float) (acc : float) =
    if v > (v1 -. 0.09) && v < (v1 +. 0.09) then
      acc
    else if v < v1 then
      integrate_helper
        f
        (Float.add v 0.1)
        v1
        (Float.add acc (trapezoid f v (Float.add v 0.1)))
    else
      integrate_helper
        f
        (Float.add v (- 0.1) )
        v1
        (Float.add acc (trapezoid f v (Float.add v (- 0.1))))

  (** [integrate v] integrates the first element in [v] from lower bound to 
      upper bound where lower bound is the second element of [v] and upper 
      bound is the third element of [v]
      Requires: [v] is of length = 3 and elements should be in order of types
      function, float, float *)
  let integrate (v : value list) =
    VFloat (integrate_helper
              (List.nth v 0)
              (List.nth v 1 |> unwrap_float)
              (List.nth v 2 |> unwrap_float)
              (0.0))

  (** [unwrap_string v] is s if [v] is String of s. Fails with "Type error, 
      expected string " otherwise *)
  let unwrap_string v =
    match v with
    | String s -> s
    | _ -> failwith "Type error, expected string"

  let initial_env =

    Env.merge
      (fun key a b ->
         match (a, b) with
         | (Some _, Some _) -> failwith "You have conflicting imports"
         | (Some _, None) -> a
         | (None, Some _) -> b
         | (None, None) -> failwith "This cannot happen"
      )
      Imports.functions_map
      (Env.empty
       |> Env.add "graph" (Extern (GExtFun (graph)))
       |> Env.add "deriv" (Extern (ExtFun (derivative)))
       |> Env.add "integ" (Extern (ExtFun (integrate)))
       |> Env.add "pi" (VFloat 3.14) 
       |> Env.add "matrix" (Extern (MExtFun (eval_matrix))))

  let run = fun () ->
    print_string
      "Welcome to CAMLCALC...type \"help\" if you are unsure of where to begin\n";
    main () (initial_env)
end