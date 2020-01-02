open Ast

(** An abstract module type that is meant to structure the CFU modules 
    (ie. Aritmetic functions, calculus functions, statistics functions).
    A module that matches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig

  (** An [operation_list] is an association list that maps operation 
      symbols to functions *)
  val operation_list : (string * ( value  list -> value )) list

end


(** A module that implements the functions needed for the trigonometric cfu.
    A module that matches [Trigonometric_Funcs] is suitable for use in
    [Trigonometric_CFU]. *)
module type Trigonometric_Funcs = sig

  (** [deg_to_rad f] is the radian representation of [f] *)
  val deg_to_rad : value list -> value

  (** [deg_to_rad f] is the degree representation of [f] *)
  val rad_to_deg : value list -> value

  (** [sin x] is sin [x] *)
  val sin : value list -> value

  (** [cos x] is cos [x] *)
  val cos : value list -> value

  (** [tan x] is tan [x] *)
  val tan : value list -> value

  (** [sec x] is sec [x] *)
  val sec : value list -> value

  (** [cosec x] is cosecant [x] *)
  val cosec : value list -> value

  (** [cotan x] is cotangent [x] *)
  val cotan : value list -> value

end
(** [Trigonometric_Functions] implements the function values defined in 
    [Trigonometric_Funcs]. This module contains the Trigonometric 
    functionalities of CAMLCALC
*)
module Trigonometric_Functions : Trigonometric_Funcs

(** [Trigonometric_CFU] is a module of type [CFU_sig] that contains the 
    [operation_list] which maps the operation symbols to the Trigonometric 
    Functions *)
module Trigonometric_CFU : CFU_sig