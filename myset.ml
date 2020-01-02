open Ast

module type CFU_sig = sig
  val operation_list : (string * ( value list -> value)) list
end

module type MySet_Funcs = sig
  val intersect : value list -> value
  val union : value list -> value
  val difference : value list -> value
  val append_val : value list -> value
  val remove : value list -> value
  val length : value list -> value
  val get_element : value list -> value val isEqual : value list -> value
  val isSubset :value list -> value
  val isDisjoint : value list -> value
end

module MySet_Functions : MySet_Funcs = struct

  (** [unwrap_float v] is the float extracted from value [v] *)
  let unwrap_float (v : value) =
    match v with
    | VFloat x -> x
    | _ -> failwith "This cannot occur - myset.ml"

  (** [unwrap_row v] is the row extracted from value [v] *)
  let unwrap_row (v : value) : float array =
    match v with
    | VRow x -> x
    | _ -> failwith "This cannot occur - myset.ml"

  let helper' lst =
    Array.of_list lst

  (** [helper short long acc] is a list containing the elements present
        in both [short] and [long] *)
  let rec helper short long acc =
    match acc with
    |0 -> []
    |i -> let v = long.(i-1) in if (Array.mem v short) 
      then v :: helper short long (acc -1 ) else  helper short long (acc -1 )



  (** [intersect_helper s1 s2] is the array containing the elements present 
        in both [s1] and [s2] *)
  let intersect_helper s1 s2 =
    let shortest_ar = if ((Array.length s1) > (Array.length s2)) then
        (s2) else (s1) in
    let longest_ar = if ((Array.length s1) > (Array.length s2)) then
        (s1) else (s2) in
    helper' (helper shortest_ar longest_ar (Array.length longest_ar))

  let intersect (v : value list) =
    let s1 = List.nth v 0 |> unwrap_row in
    let s2 = List.nth v 1 |> unwrap_row in
    VRow (intersect_helper s1 s2)


  (**  [difference_helper s1 s2 acc] is the list of elements contained in 
       [s1] and not in [s2] *)
  let rec difference_helper s1 s2 acc =
    match acc with
    |0 -> []
    |i -> let v = s1.(i-1) in if not (Array.mem v s2) then
        v :: difference_helper s1 s2 (acc -1 ) else
        difference_helper s1 s2 (acc -1 )

  let difference v =
    let s1 = List.nth v 0 |> unwrap_row in
    let s2 = List.nth v 1 |> unwrap_row in
    let lst = (difference_helper s1 s2 (Array.length s1)) in
    VRow (Array.of_list lst)

  (** [union_helper s1 s2] is the array with elements contained in both [s1] and 
      [s2] *)
  let union_helper s1 s2 =
    let l1 = Array.to_list s1 in
    let l2 = Array.to_list s2 in
    let l3 = l1 @ l2 in
    let l4 = List.sort_uniq compare l3 in
    Array.of_list l4

  let union v =
    let s1 = List.nth v 0 |> unwrap_row in
    let s2 = List.nth v 1 |> unwrap_row in
    VRow (union_helper s1 s2)

  let append_val (v : value list) =
    match v with
    | hd1::hd2::tl ->
      let (hd1', hd2') = (unwrap_row hd1, unwrap_float hd2) in
      VRow (hd1'
            |> Array.to_list
            |> List.rev
            |> List.cons hd2'
            |> List.rev
            |> Array.of_list)
    | _ -> failwith "InvalidInput"

  let rec remove_helper v l = 
    List.filter (fun x -> x<>v) l

  let remove (v: value list) = 
    match v with 
    |h1::h2::[] ->
      let v = h1 |> unwrap_row |>Array.to_list |> remove_helper (unwrap_float h2) 
              |> Array.of_list in 
      VRow v
    | _ -> failwith "Invalid Input"

  let length (v : value list) =
    match v with
    | hd1::tl ->
      let hd1' = (hd1 |> unwrap_row |> Array.to_list) in
      VFloat (hd1'
              |> List.length
              |> float_of_int)
    | _ -> failwith "InvalidInput"

  let get_element (v : value list) =
    match v with
    | hd1::hd2::tl -> 
      let (hd1', hd2') = 
        ((hd1 |> unwrap_row |> Array.to_list), unwrap_float hd2) in
      if(int_of_float hd2' >= List.length hd1') 
      then failwith "Please enter a valid index"
      else VFloat (List.nth hd1' (int_of_float hd2'))
    | _ -> failwith "InvalidInput"
  (** [unwrap_float v] is the float extracted from value [v] *)

  (** [check_elements_helper x y int bool] checks whether every element in x is present in y and
       returns [bool] true if so and false if otherwise *)
  let rec check_elements_helper x y int bool= 
    match int with 
    |0 -> bool
    |i -> if (Array.mem x.(i-1) y)
      then check_elements_helper x y (i-1) (true&&bool) 
      else false

  (** [isEqual_helper x y] is true if two sets contain structurally 
      equal elements and have the same number of elements and 
      is false otherwise*)
  let isEqual_helper x y =
    if (Array.length x = Array.length y) then 
      check_elements_helper x y (Array.length x) true
    else false

  let isEqual v = 
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in 
    VBool (isEqual_helper s1 s2)

  (** [isSubset x y] returns true if all elements in [x] is also there in [y] *)
  let isSubset v = 
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in 
    VBool(check_elements_helper s1 s2 (Array.length s1) true)

  let rec check_elements_helper_2 x y int bool= 
    match int with 
    |0 -> bool
    |i -> if (Array.mem x.(i-1) y) 
      then  false
      else check_elements_helper_2 x y (i-1) (true&&bool) 

  (** [isDisjoint x y] returns true if none of the elements in [x] are
      in [y] and returns false otherwise*)
  let isDisjoint v = 
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in 
    VBool (check_elements_helper_2 s1 s2 (Array.length s1) true)
end

module MySet_CFU  = struct

  let operation_list = [
    ("intersect", MySet_Functions.intersect);
    ("difference", MySet_Functions.difference);
    ("union", MySet_Functions.union);
    ("append", MySet_Functions.append_val);
    ("remove_val", MySet_Functions.remove);
    ("length", MySet_Functions.length);
    ("get_elem", MySet_Functions.get_element);
    ("is_equal", MySet_Functions.isEqual);
    ("is_disjoint", MySet_Functions.isDisjoint);
    ("is_subset", MySet_Functions.isSubset);
  ]

end