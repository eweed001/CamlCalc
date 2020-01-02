(** THE CAMLCALC ABSTRACT SYNTAX TREE TYPE*)

(** [Env] is a module that contains an environment implemented as an OCaml Map 
    with keys of type string and values of type [Ast.value] *)
module Env = Map.Make (String)

(** [env] is an Environment type defined as an OCaml Map with keys of type 
    string and values of type [Ast.value] *)
type env = value Env.t

(** [value] is the type of CAMLCALC values *)
and value =
  |VBool of bool
  |VFloat of float
  |VString of string
  |VBinop of string
  |VId of id
  |Closure of id list * expr * env 
  |Extern of fun_ext
  |VMatrix of float array array
  |VFloatList of float list
  |VRow of float array

(** [matrix_type] is the type of CAMLCALC matrix values *)
and matrix_type = value list -> env -> value

(** [set_type] is the type of CAMLCALC sets values *)
and set_type = float array -> float array  -> float array

(** [graph_type] is the type of CAMLCALC graphing values *)
and graph_type = value * value * value * env -> expr

(** [ext_type] is the type of the built in external functions within CAMLCALC *)
and ext_type = value list -> value

(** [fun_ext] is the variant type containing the types of the external 
    functions built into CAMLCALC *)
and fun_ext = 
  | ExtFun of ext_type
  | GExtFun of graph_type
  | MExtFun of matrix_type

(** The types [id], [bop], [unop] are used by the parser to recognize variable 
    name identifiers, binary operations and unary operations *)
and
  id = string

and
  bop =
  |Func of string

and 
  unop = 
  |Func_u of string

(** [expr] is the type of the AST for expressions parsed by CAMLCALC *)
and expr =
  |Boolean of bool
  |Var of id
  |String of string
  |Float of float
  |Binop of bop * expr * expr
  |Sequence of expr * expr
  |DSequence of defn * expr 
  |Unop of unop * expr
  |If of expr * expr * expr
  |Let of id * expr * expr
  |Fun of string list * expr
  |FunApp of expr * expr list
  |Arr of expr list

(** [defn] is the type of the AST for definitions parsed by CAMLCALC  *)
and defn = 
  |DLet of string * expr

(** [phrase] is the type of the AST for phrases parsed by CAMLCALC *)
and phrase = 
  | Expr of expr
  | Defn of defn