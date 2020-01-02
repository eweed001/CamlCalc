open Ast

module type CFU_sig = sig
  val operation_list : (string * ( value list -> value )) list
end

module type Statistics_Funcs = sig
  val mean : value list -> value
  val median : value list -> value
  val standard_deviation : value list -> value
  val range : value list -> value
  val minimum : value list -> value
  val maximum : value list -> value
  val permutations : value list -> value
  val combinations : value list -> value
end

module Statistics_Functions : Statistics_Funcs = struct

  (** [unwrap_float v] is the float extracted from value [v] *)
  let unwrap_float (v : value) =
    match v with
    | VFloat x -> x
    | _ -> failwith "This cannot occur - matrix.ml"

  (** [unwrap_row v] is the row extracted from value [v] *)
  let unwrap_row (v : value) : float array =
    match v with
    | VRow x -> x
    | _ -> failwith "This cannot occur - matrix.ml"

  (** [mean_unwrap f] evaluates the mean of a float list [f] *)
  let mean_unwrap (f : float list) =
    (List.fold_left (fun acc x -> acc +. x) 0. f) /.
    (float_of_int(List.length f))

  let mean (v : value list) =
    match v with
    | hd::tl -> let s = (hd |> unwrap_row |> Array.to_list) in
      VFloat ((List.fold_left (fun acc x -> acc +. x) 0. s) /.
              (float_of_int(List.length s)))
    | _ -> failwith "InvalidInput"

  let median (v : value list) =
    match v with
    | hd::tl -> let s = (hd |> unwrap_row |> Array.to_list) in
      let s_new = List.sort compare s  in
      let len = List.length s_new in
      let mid = List.nth s_new (len/2) in
      if (len mod 2<>0) then VFloat mid else
        let mid2 = List.nth s_new ((len-1)/2) in
        VFloat ((mid+.mid2)/.2.)
    | _ -> failwith "InvalidInput"


  let standard_deviation (v : value list) =
    match v with
    | hd::tl -> let s = (hd |> unwrap_row |> Array.to_list)in
      let m = (v |> mean |> unwrap_float) in
      VFloat (s
              |> List.map (fun x -> (x -. m) *. (x -. m))
              |> mean_unwrap
              |> Float.sqrt)
    | _ -> failwith "InvalidInput"

  (** [min_helper s min_acc] goes through [s] and finds the minimum value within
      it  *)
  let rec min_helper (s : float list) min_acc=
    match s with
    | [] -> min_acc
    | h::t -> if (h<min_acc) then min_helper t h else min_helper t min_acc

  let minimum (v : value list) =
    match v with
    | hd::tl -> let s = (hd |> unwrap_row |> Array.to_list) in
      VFloat (min_helper s max_float)
    | _ -> failwith "InvalidInput"

  (** [max_helper s min_acc] goes through [s] and finds the maximum value within
        it  *)
  let rec max_helper  (s : float list) max_acc=
    match s with
    | [] -> max_acc
    | h::t -> if (h>max_acc) then max_helper t h else max_helper t max_acc

  let maximum (v : value list) =
    match v with
    | hd::tl -> let s = (hd |> unwrap_row |> Array.to_list) in
      VFloat (max_helper s min_float)
    | _ -> failwith "InvalidInput"

  let range (v : value list) =
    match v with
    | hd::tl ->
      VFloat ((unwrap_float (maximum v)) -. (unwrap_float (minimum v)))
    | _ -> failwith "InvalidInput"

  (** [factorial n] calculates n! (n factorial) *)
  let rec factorial (n : float) =
    match n with
    | 0. -> 1.
    | _ -> n *. (factorial (n -. 1.))

  let permutations (v : value list) =
    match v with
    |[] -> failwith "wrong number of arguments"
    |h1::[] -> failwith "wrong number of arguments"
    |n::r::[] -> if ((unwrap_float n)<(unwrap_float r)) then 
        failwith "First argument must be greater"
      else VFloat ((factorial (unwrap_float n)) /. 
                   (factorial ((unwrap_float n) -. (unwrap_float r))))
    |_ -> failwith "wrong number of arguments"

  let combinations (v : value list) =
    match v with
    |[] -> failwith "wrong number of arguments"
    |h1::[] -> failwith "wrong number of arguments"
    |n::r::[] -> if ((unwrap_float n)<(unwrap_float r)) then 
        failwith "First argument must be greater" else
        VFloat ((factorial (unwrap_float n)) /. 
                ((factorial ((unwrap_float n) -.
                             (unwrap_float r))) *.
                 (factorial (unwrap_float r))))
    |_ -> failwith "wrong number of arguments"
end

module Statistics_CFU = struct
  type primitive = float

  let operation_list = [
    ("mean", Statistics_Functions.mean);
    ("median", Statistics_Functions.median);
    ("stdev", Statistics_Functions.standard_deviation);
    ("min", Statistics_Functions.minimum);
    ("max", Statistics_Functions.maximum);
    ("range", Statistics_Functions.range);
    ("perm", Statistics_Functions.permutations);
    ("comb", Statistics_Functions.combinations)
  ]

end