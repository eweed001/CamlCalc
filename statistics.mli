open Ast

(** An abstract module type that is meant to structure the CFU modules 
    (ie. Aritmetic functions, calculus functions, statistics functions). 
    A module that matches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig

  (** An [operation_list] is an association list that maps operation symbols to 
      functions *)
  val operation_list : (string * ( value list -> value )) list
end

module type Statistics_Funcs = sig
  (** [mean v] calculates the mean of the list of values [v] *)
  val mean : value list -> value

  (** [median v] calculates the median of the list of values [v] *)
  val median : value list -> value

  (** [standard_deviation v] calculates the standard deviation of the list of
      values [v] *)
  val standard_deviation : value list -> value

  (** [range v] calculates the range of the list of values [v] *)
  val range : value list -> value

  (** [minimum v] is the minimum value in the list of values [v] *)
  val minimum : value list -> value

  (** [maximum v] is the maximum value in the list of values [v] *)
  val maximum : value list -> value

  (** [permutations v] is the nPr calculation where n is the first element of 
      [v] and r is the second element of [v] 
      Requires: n > r *)
  val permutations : value list -> value

  (** [combinations v] is the nCr calculation where n is the first element of 
      [v] and r is the second element of [v] 
      Requires: n > r *)
  val combinations : value list -> value
end

(** [Statistics_Functions] implements the function values defined in 
    [Statistics_Funcs]. It has all the Statistics Functionalities of CAMLCALC *)
module Statistics_Functions : Statistics_Funcs

(** [Statistics_CFU] is of type [CFU_sig] and contains an [operation_list] that 
    maps operation symbols to the statistics functions. *)
module Statistics_CFU : CFU_sig