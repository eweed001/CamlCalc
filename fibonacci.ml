open Ast

module type CFU_sig =  sig
  val operation_list : (string * ( value list -> value )) list
end

module type Fib_Funcs = sig
  val nth : value list -> value
  val lst : value list -> value
  val nfib_list : value list -> value
end


module Fib_Functions : Fib_Funcs  = struct 

  (** [unwrap v] is the float extracted from value [v] 
        requires: [v] has type VFloat *)
  let unwrap v =
    match v with
    | VFloat x -> x
    | _ -> failwith "This cannot occur - arithmetic.ml"

  (** [sum] is the sum of the first n elements of [lst] *)
  let rec sum n lst = 
    if n = 0. then 0. else
      match lst with 
      |[] -> 0.
      |h :: t -> h +. sum (n-.1.) t

  (** [nacci_help n k lst] is the first [k] 
      elements of the [n]-step fibonacci sequence *)
  let rec nacci_help n k lst = 
    if k = 0. then List.rev lst 
    else
      let x = (sum n lst) :: lst in 
      nacci_help n (k-.1.) x


  (**  [nacci n k] is the first [k] elements of the [n]-step fibonacci sequence
       Requires n , k > 0 *)
  let nacci n k =
    match k with
    | 1. -> [1.]
    | _ -> nacci_help n (k-.1.) [1.]

  (** [nfib_list_helper a b] is the array of the fist [b] elements of the 
      [a]-step fibonacci sequence *)
  let nfib_list_helper a b = 
    let lst = nacci (a) (b) in 
    Array.of_list lst 

  let nfib_list v = 
    let a = List.nth v 0 |> unwrap in
    let b = List.nth v 1 |> unwrap in 
    VRow (nfib_list_helper a b)

  (** [nth_element v] is the [v]th element of the fibonacci sequence *)
  let rec nth_element v  = 
    match v with 
    |0. |1. as n -> n
    |n -> nth_element(n-.1.) +. nth_element(n-.2.)

  let nth v = 
    let n = List.nth v 0 |> unwrap in
    let i = nth_element (n) in 
    VRow [|i|]

  (** [lst_helper v] is the list representation of the first [v] elements of the
      fibonacci sequence *)
  let rec lst_helper v = 
    match v with 
    |0. | 1. -> 1. :: []
    |n -> (nth_element (n-.1.) +. (nth_element (n-.2.))) :: (lst_helper (v-.1.))

  let lst v = 
    let n = List.nth v 0 |> unwrap in
    let lst' = lst_helper n in 
    let lst'' = List.rev lst' in 
    VRow (Array.of_list lst'')
end 


module Fib_CFU : CFU_sig = struct
  let operation_list = [
    ("nth", Fib_Functions.nth);
    ("fib", Fib_Functions.lst);
    ("nfib", Fib_Functions.nfib_list);
  ]
end