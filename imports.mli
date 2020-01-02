open Ast

(** An abstract module type that is used to find functions from the various 
    modules in the project (ie. Aritmetic functions, calculus functions, 
    statistics functions). 
    A module that matches [Imports_Sig] is suitable for use in the [Main] 
    module. *)
module type Imports_Sig = sig

  (** [find_function s] is the operation that is associated with [s] 
      in the operationlist *)
  val find_function : string -> (value list -> value)

  (** [functions_map] is an OCaml Map that contains keys of type string and 
      values of type [Ast.value]. It maps operation symbols to External 
      functions *)
  val functions_map : env
end

(** [Imports] is a module that implements the function values defined in 
    [Imports_Sig]. It contains the functionalities to be used to find operations
    across all the modules in the directory *)
module Imports : Imports_Sig