open Arithmetic
open Trigonometric
open Statistics
open Ast
open Matrix
open Myset
open Fibonacci

module type Imports_Sig = sig
  val find_function : string -> (value list -> value)
  val functions_map : env
end

module Imports = struct
  (** [stats] is the [operation_list] from the [Statistics] module *)
  let stats = Statistics_CFU.operation_list

  (** [arith] is the [operation_list] from the [Arithmetic] module *)
  let arith = Arithmetic_CFU.operation_list

  (** [trig] is the [operation_list] from the [Trigonometric] module *)
  let trig = Trigonometric_CFU.operation_list

  (** [matr] is the [operation_list] from the [Matrix] module *)
  let matr = Matrix_CFU.operation_list

  (** [set] is the [operation_list] from the [Myset] module *)
  let set = MySet_CFU.operation_list

  (** [fib] is the [operation_list] from the [Fibonacci] module *)
  let fib = Fib_CFU.operation_list

  (** [cfu_list] is the list of all the operation lists defined above *)
  let cfu_list = [arith;trig;matr;set;fib;stats]

  (** [operation_list] contains all the functions from the operation lists from
      all the modules used *)
  let operation_list = List.append cfu_list [] |> List.flatten

  (** [map_of_functions operations acc] returns the environment that binds 
      operation symbols to external functions from [operations] *)
  let rec map_of_functions operations acc =
    match operations with
    | [] -> acc
    | h::t ->
      let name = fst h in
      let f = snd h in
      let env' = Env.add name (Extern (ExtFun f)) acc in
      map_of_functions t env'

  (** [functions_map] is the environment created containing all external 
      functions of CAMLCALC *)
  let functions_map = map_of_functions operation_list Env.empty

  let find_function (identifier : string) =
    match List.assoc_opt identifier operation_list with
    |Some f -> f
    |None -> failwith (identifier^" is not a valid imported function")
end