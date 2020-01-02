open Ast

module type CFU_sig = sig
  val operation_list : (string * ( value  list -> value )) list
end

module type Trigonometric_Funcs = sig
  val deg_to_rad : value list -> value
  val rad_to_deg : value list -> value
  val sin : value list -> value
  val cos : value list -> value
  val tan : value list -> value
  val sec : value list -> value
  val cosec : value list -> value
  val cotan : value list -> value
end

module Trigonometric_Functions : Trigonometric_Funcs = struct

  let unwrap v =
    match v with
    | VFloat x -> x
    | _ -> failwith "This cannot occur - arithmetic.ml"

  let deg_to_rad (f : value list) =
    match f with
    | hd :: tl ->
      let (hd', tl') = (unwrap hd, unwrap hd) in
      VFloat ((Float.pi *. hd')/. 180.)
    | _ -> failwith "InvalidInput"

  let rad_to_deg (f : value list) =
    match f with
    | hd :: tl -> 
      let (hd', tl') = (unwrap hd, unwrap hd) in
      VFloat (Float.div (hd' |> Float.mul 180.) Float.pi)
    | _ -> failwith "InvalidInput"

  let sin (f : value list) =
    match f with
    | hd :: tl -> 
      let (hd', tl') = (unwrap hd, unwrap hd) in
      if (int_of_float hd' mod 180 = 0) then VFloat 0. else
        let x = deg_to_rad [hd] |> unwrap in
        VFloat (Float.sin x)
    | _ -> failwith "InvalidInput"

  let cos (f : value list) =
    match f with
    | hd :: tl -> 
      let (hd', tl') = (unwrap hd, unwrap hd) in
      if (int_of_float hd' mod 90 = 0 && int_of_float hd' mod 180 <> 0) then 
        VFloat 0.
      else let x = deg_to_rad [hd] |> unwrap in
        VFloat (Float.cos x)
    | _ -> failwith "InvalidInput"

  let tan (f : value list) =
    match f with
    | hd :: tl -> if (int_of_float (unwrap hd) mod 180 = 0) then VFloat 0. 
      else if (int_of_float (unwrap hd) mod 90 = 0) then failwith "undefined" 
      else let x = deg_to_rad [hd] |> unwrap in
        (VFloat (Float.tan x))
    | _ -> failwith "InvalidInput"


  let sec (f : value list) =
    match f with
    | hd :: tl ->
      let (hd', tl') = (unwrap hd, unwrap hd) in
      if (int_of_float hd' mod 90 = 0 && int_of_float hd' mod 180 <> 0) then 
        failwith "undefined" 
      else let x = deg_to_rad [hd] |> unwrap in
        VFloat (Float.div 1. (Float.cos x))
    | _ -> failwith "InvalidInput"

  let cosec (f : value list) =
    match f with
    | hd :: tl -> 
      let (hd', tl') = (unwrap hd, unwrap hd) in
      if (int_of_float hd' mod 180 = 0 ) then failwith "undefined"
      else let x = deg_to_rad [hd] |> unwrap in
        VFloat (Float.div 1. (Float.sin x))
    | _ -> failwith "InvalidInput"

  let cotan (f : value list) =
    match f with
    | hd :: tl -> if (int_of_float (unwrap hd) mod 90 = 0 &&
                      int_of_float (unwrap hd) mod 180 <> 0) then (VFloat 0. )
      else if ( int_of_float (unwrap hd) mod 180 = 0) then failwith "undefined"
      else let x = deg_to_rad [hd] |> unwrap in
        (VFloat (Float.div 1. (Float.tan x)))
    | _ -> failwith "InvalidInput"
end

module Trigonometric_CFU : CFU_sig = struct

  let operation_list = [
    ("deg", Trigonometric_Functions.deg_to_rad);
    ("rad", Trigonometric_Functions.rad_to_deg);
    ("sin", Trigonometric_Functions.sin);
    ("cos", Trigonometric_Functions.cos);
    ("tan", Trigonometric_Functions.tan);
    ("sec", Trigonometric_Functions.sec);
    ("csc", Trigonometric_Functions.cosec);
    ("cot", Trigonometric_Functions.cotan);
  ]


end
