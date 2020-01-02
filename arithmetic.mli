open Ast

(** An abstract module type that is meant to structure the CFU modules 
    (ie. Aritmetic functions, calculus functions, statistics functions). 
    A module that matches [CFU_sig] is suitable for use in the [Imports] 
    module. *)
module type CFU_sig = sig

  (** An [operation_list] is an association list that maps operation 
      symbols to functions *)
  val operation_list : (string * ( value  list -> value )) list

end

(** A module that implements the functions needed for the arithmetic cfu. 
    A module that matches [Arithmetic_Funcs] is suitable for use in
     [Arithmetic_CFU]. *)
module type Arithmetic_Funcs = sig

  (** [add s] is the result of adding the first element of [s] to the
      second element of [s]. *)
  val add : value list -> value

  (** [subtract s] is the result of subtracting the first element of [s]
      from the second element of [s]. *)
  val subtract : value list -> value

  (** [multiply s] is the result of multiplying the first element of [s]
      by the second element of [s]. *)
  val multiply : value list -> value

  (** [divide s] is the result of dividing the first element of [s] by 
      the second element of [s]. *)
  val divide : value list -> value

  (** [exponentiation s] is the result of raising the first element of [s] to
        the second element of [s]. *)
  val exponentiation : value list -> value

  (** [modulus s] is the result of moding the first element of [s] by the 
      second element of [s]. *)
  val modulus : value list -> value


  (** [logarithm s] is the result of the logarithm of the second element of [s]
      with the base as the first element of [s] *)
  val logarithm : value list -> value


  (** [equal_to s] is the result of comparing the first element of [s] to the 
      second element of [s] for equality *)
  val equal_to : value list -> value
end

(** A module that implements all the function values defined in module type 
    [Arithmetic_Funcs]. This module contains all the arithmetic operations of 
    the calculator *)
module Arithmetic_Functions : Arithmetic_Funcs

(** A module that is of type [CFU_sig] that contains an [operation_list] that 
    maps operation symbols to functions *)
module Arithmetic_CFU : CFU_sig