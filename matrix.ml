open Ast

module type CFU_sig = sig

  val operation_list : (string * ( value list -> value)) list

end

module type Matrix_Funcs = sig
  (* type matrix *)
  val make_matrix : value list -> value

  val add_matrix : value list -> value

  val sub_matrix : value list -> value

  val dot_product_matrix : value list -> value

  val echelon_form : value list -> value 

  val reduced_echelon_form : value list -> value

  val determinant : value list -> value

  val transpose : value list -> value

  val identity_matrix : value list -> value

  val matrix_constant_multiply : value list -> value

  val matrix_augment : value list -> value

  val nth_col : value list -> value

  val eigenvectors : value list -> value

  val one_solution : value list -> value

  val base_matrix : value list -> value

end

module Matrix_Functions : Matrix_Funcs = struct

  (**[unwrap_float v] is the float contained by the first value in [v]*)
  let unwrap_float (v : value) =
    match v with
    | VFloat x -> x
    | _ -> failwith "The arguments given are of the wrong type"

  (**[unwrap_matrix v] is the matrix contained by the first value in [v]*)
  let unwrap_matrix (v : value) =
    match v with
    | VMatrix x -> x
    | _ -> failwith "The arguments given are of the wrong type"

  (**[unwrap_row v] is the row contained by the first value in [v]*)
  let unwrap_row (v : value) =
    match v with
    | VRow x -> x
    | _ -> failwith "The arguments given are of the wrong type"

  (**[make_matrix_helper x y] is a matrix of [x] rows
     and [r] colums of floats initialized to 0.0 *)
  let make_matrix_helper (x : int) (y : int) =
    if x == 0 || y == 0 then 
      failwith "A matrix of valid dimensions must be supplied"
    else
      Array.make_matrix x y 0.0

  let make_matrix (v : value list) =
    let x = List.nth v 0 |> unwrap_float |> int_of_float in 
    let y = List.nth v 1 |> unwrap_float |> int_of_float in 
    VMatrix (make_matrix_helper x y)

  (**[add_matrix_helper x1 y2] is the matrix obtained by adding matrix [y1]
     from [x1]*)
  let add_matrix_helper (x1:float array array) (y1:float array array) = 
    let x_row_length = Array.length x1 in 
    let x_column_length = Array.length (Array.get x1 0) in 
    if (x_row_length <> (Array.length y1) ||
        x_column_length <> (Array.length (Array.get y1 0) )) then
      failwith "Invalid input - matrices must be the same size" else
      let new_matrix = make_matrix_helper x_row_length x_column_length in 
      let () = 
        for i = 0 to (x_row_length - 1) do 
          for j = 0 to (x_column_length - 1) do
            new_matrix.(i).(j) <- x1.(i).(j) +. y1.(i).(j)
          done
        done in new_matrix

  let add_matrix (v : value list) =
    let x = List.nth v 0 |> unwrap_matrix in
    let y = List.nth v 1 |> unwrap_matrix  in
    VMatrix (add_matrix_helper x y)

  (**[sub_matrix_helper x1 y1] is the matrix obtained by subtracting matrix [y1]
     from [x1]*)
  let sub_matrix_helper (x1:float array array) (y1:float array array) = 
    let x_row_length = Array.length x1 in 
    let x_column_length = Array.length (Array.get x1 0) in 
    if (x_row_length <> (Array.length y1) ||
        x_column_length <> (Array.length (Array.get y1 0) )) then
      failwith "Invalid input - matrices must be the same size" else
      let new_matrix = make_matrix_helper x_row_length x_column_length in 
      let () = 
        for i = 0 to (x_row_length - 1) do 
          for j = 0 to (x_column_length - 1) do
            new_matrix.(i).(j) <- x1.(i).(j) -. y1.(i).(j)
          done
        done in new_matrix

  let sub_matrix (v : value list) = 
    let x1 = List.nth v 0 |> unwrap_matrix in
    let y1 = List.nth v 1 |> unwrap_matrix in
    VMatrix (sub_matrix_helper x1 y1)

  (**[helper_sum_product row_product col_product acc] is the product of
     the elements of the rows [row_product] and [col_product] at indexes [acc]*)
  let rec helper_sum_product row_product col_product acc= 
    match acc with 
    |0 -> 0.
    |_ ->(row_product.(acc-1)*.col_product.(acc-1)) +. 
         (helper_sum_product row_product col_product (acc-1))

  (**[helper_extract row_num col_num matr1 matr2] is the product of the row
     at [row_num] and column [col_num] of matrices [matr1] and [matr2], 
     respectively.
     Requires: 
     [row_num] is a valid row number in [matr1] and [col_num] is a valid column
     number of [matr2]*)
  let helper_extract row_num col_num matr1 matr2 = 
    let row_product = Array.get matr1 row_num in 
    let col_product = Array.make (Array.length row_product) 0. in
    let () = for i = 0 to (Array.length row_product) - 1 do 
        col_product.(i) <- matr2.(i).(col_num)
      done in 
    helper_sum_product row_product col_product ((Array.length row_product))

  (**[dot_product_matrix_helper x1 y1] is the product of matrices [x1] and [y1]
     given by the row-column (dot product) computation rule of matrix 
     multiplication.*)
  let dot_product_matrix_helper (x1:float array array) (y1:float array array) = 
    let x_row_length = Array.length x1 in 
    let x_column_length = Array.length (Array.get x1 0) in 
    let y_column_length = Array.length (Array.get y1 0) in 
    if (x_column_length <> (Array.length y1)) then
      failwith "Please make sure the matrices follow dimensions M x N, N x R" 
    else
      let new_matrix = make_matrix_helper x_row_length y_column_length in 
      let () = 
        for i = 0 to (x_row_length - 1) do 
          for j = 0 to (y_column_length - 1) do
            new_matrix.(i).(j) <- helper_extract i j x1 y1 
          done
        done in new_matrix

  let dot_product_matrix (v : value list) = 
    let x1 = List.nth v 0 |> unwrap_matrix in
    let x2 = List.nth v 1 |> unwrap_matrix in
    VMatrix (dot_product_matrix_helper x1 x2)

  (**[fuzzy_compare a b] is the boolean indicating whether or not float [a] is
     equal to float [b] taking into account a 0.001 threshhold for floating point
     error*)
  let fuzzy_compare a b = 
    if a >= b -. 0.1 && a <= b +. 0.1 then 1 else 0

  (**[pivot_pos r1] is the position of the pivot column in row [r1], counting
     from left to right, starting at 0*)
  let pivot_pos r1 = 
    let col_num = (Array.length r1) -1 in
    let p = ref (-1) in
    let _ = for c = col_num downto 0 do
        if (fuzzy_compare r1.(c) 0.) == 0 then p := c else ()
      done in !p

  (**[balance_row r1] is r1 multiplied by a scalar such that the pivot is equal
     to 1.0*)
  let balance_row r1 = 
    let p_pos = pivot_pos r1 in
    if p_pos == -1 then 0. else
      let col_num = (Array.length r1) -1 in
      let p = r1.(p_pos) in
      let _ = 
        for c = p_pos to col_num do
          r1.(c) <- (r1.(c) /. p)
        done 
      in p

  (**[reduce_row x1 r r'] is the matrix [x1] where its row at [r] has been
     modified to have its pivot set to 0.0 using a scalar multiplication and
     row substitution. No conceptual effect on determinant of [x1]*)
  let reduce_row x1 r r' = 
    let col_num = Array.length (Array.get x1 0) -1 in
    let p = pivot_pos x1.(r') in 
    if p == -1  then () else
      let factor =  -1. *. (x1.(r).(p) /. x1.(r').(p)) in 
      let _ =
        for c = 0 to col_num do 
          x1.(r).(c) <- (x1.(r).(c) +. x1.(r').(c) *.factor)
        done 
      in ()

  (**[reverse_rows x1] is the matrix x1 with rows reversed, such that if n is the 
     number of rows of the matrix and 0 <= i <= n, the ith row of [x1] becomes the 
     (n-i)th row*)
  let reverse_rows x1 =
    let row_num = (Array.length x1) - 1 in
    let col_num = Array.length (Array.get x1 0) - 1 in
    let x1' = make_matrix_helper (row_num + 1) (col_num + 1) in 
    let _ = for r = 0 to row_num do
        x1'.(row_num - r) <- x1.(r)
      done
    in
    x1'

  (**[pivot_sort x1] is [(x2, d)], where [x2] is [x1] sorted least to greatest,
     top-down, according to its pivot positions, and where [d] is the scalar 
     representing changes to the determinant after sorting.*)
  let pivot_sort x1 = 
    let d = ref 1. in
    let _ = Array.stable_sort
        (fun row1 row2 ->
           let p1 = pivot_pos row1 in
           let p2 = pivot_pos row2 in
           if p1 == -1 then 
             let _ =  d := !d *. -1. in 1 
           else
           if p2 == -1 then 
             let _ =  d := !d *. -1. in -1 
           else
             (p1) - (p2)) x1
    in (x1, !d)

  (**[echelon_form_helper x1] is [(x2, d)] where [x2] is the matrix [x1] 
     after row-reduction into echelon form, and where [d] is the scalar 
     representing changes to the determinant of [x1] *)
  let echelon_form_helper x1 = 
    let row_num = (Array.length x1) -1 in
    let d = ref 1. in
    let d' = balance_row x1.(0) in
    let _ = d := !d *. d' in
    let _ = 
      for r' = 0 to row_num do
        for r = r'+1 to row_num do
          let _ = reduce_row x1 r r' in
          let d' = balance_row x1.(r) in
          let _ = d := !d *. d' in ()
        done
      done
    in 
    let (_ , d'') = pivot_sort x1 in
    let _ = d := !d *. d'' in
    (x1,!d)

  let echelon_form v = 
    let x1 = List.nth v 0 |> unwrap_matrix in
    VMatrix (fst (echelon_form_helper x1))

  (**[reduced_echelon_form_helper x1] is [(x2, d)] where [x2] is the matrix [x1]
     after row-reduction into reduced echelon form, and where [d] is the scalar 
     representing changes to the determinant of [x1] *)
  let reduced_echelon_form_helper v = 
    let x1 = List.nth v 0 |> unwrap_matrix in
    let (_, d) = echelon_form_helper x1 in
    let x1' = reverse_rows x1 in
    let (_, d') = echelon_form_helper x1' in
    let determ = d *. d' in
    (x1', determ)

  let reduced_echelon_form v = 
    VMatrix (reduced_echelon_form_helper v |> fst)

  (**[diagonal_product x1 acc i in] is the product of the diagonal entries of x1
     starting at index i, ending at index n, and added to [acc]*)
  let rec diagonal_product x1 acc i n =
    if i <= n then
      let acc' = acc *. x1.(i).(i) in
      diagonal_product x1 acc' (i+1) n
    else
      acc

  let determinant v =
    let x1 = List.nth v 0 |> unwrap_matrix in
    let row_num = (Array.length x1) - 1 in
    let col_num = Array.length (Array.get x1 0) - 1 in
    if row_num == col_num then
      let x2 = Array.copy x1 in
      let v2 = VMatrix (x2) in
      let (x2', d) = reduced_echelon_form_helper [v2] in
      VFloat ((diagonal_product x2' 1. 0 col_num) *. d)
    else failwith "Cannot take determinant of non-square matrix"

  let transpose v = 
    match List.hd v with
    | VRow r -> 
      let row_num = (Array.length r) - 1 in
      let matrix_column = make_matrix_helper (row_num+1) 1 in
      let _ = 
        for i = 0 to row_num do
          matrix_column.(i).(0) <- r.(i)
        done 
      in VMatrix matrix_column
    | VMatrix m ->
      let row_num = (Array.length m) - 1 in
      let col_num = Array.length (Array.get m 0) - 1 in
      let transpose_matrix = make_matrix_helper (row_num+1) (col_num+1) in
      let _ = 
        for i = 0 to row_num do
          for j = 0 to col_num do
            transpose_matrix.(j).(i) <- m.(i).(j)
          done
        done
      in VMatrix transpose_matrix
    | _ -> failwith "Not a valid thing to transpose"

  let identity_matrix v =
    let n =  (List.nth v 0|> unwrap_float |> int_of_float) - 1 in
    let x1 = make_matrix_helper (n+1) (n+1) in
    let _ = for i = 0 to n do
        x1.(i).(i) <- 1.
      done
    in VMatrix x1

  let matrix_constant_multiply v = 
    let m = List.nth v 0 |> unwrap_matrix in
    let const = List.nth v 1 |> unwrap_float in
    let row_num = (Array.length m) - 1 in
    let col_num = Array.length (Array.get m 0) - 1 in
    let _ = for r = 0 to row_num do
        for c = 0 to col_num do
          m.(r).(c) <- m.(r).(c) *. const
        done
      done
    in VMatrix m

  (**[row_constant_multiply r const] is the new row created by multiplying the
     row [r] with the float [const]*)
  let row_constant_multiply r const = 
    let col_num = (Array.length r) - 1 in
    let new_row = Array.create_float (col_num+1) in
    let _ = 
      for c = 0 to col_num+1 do 
        new_row.(c) <- r.(c) *. const
      done
    in new_row

  let matrix_augment v = 
    let m = List.nth v 0 |> unwrap_matrix in
    let augment_column = List.nth v 1 |> unwrap_matrix in
    let row_num_m = (Array.length m) - 1 in
    let col_num_m = Array.length (Array.get m 0) - 1 in
    let augmented_matrix = make_matrix_helper (row_num_m+1) (col_num_m+2) in
    let row_num = (Array.length augmented_matrix) - 1 in
    let col_num = Array.length (Array.get augmented_matrix 0) - 1 in
    let _ = 
      for r = 0 to row_num do
        for c = 0 to col_num-1 do
          augmented_matrix.(r).(c) <- m.(r).(c)
        done;
        augmented_matrix.(r).(col_num) <- augment_column.(r).(0)
      done
    in VMatrix augmented_matrix

  let nth_col v = 
    let m = List.nth v 0 |> unwrap_matrix in
    let c = List.nth v 1 |> unwrap_float |> int_of_float in
    let row_num = (Array.length m) - 1 in
    let col_num = Array.length (Array.get m 0) in
    let column = make_matrix_helper (row_num + 1) 1 in
    if c <= (col_num - 1) && c >= 0 then
      let _ = 
        for r = 0 to row_num do
          column.(r).(0) <- m.(r).(c)
        done
      in VMatrix column
    else failwith "Attempt to access nonexistent column"

  (**[dot_product_row r1 r2] is the sum of the products of each element of
     rows [r1] and [r2]*)
  let dot_product_row r1 r2 = 
    let col_num_r1 = (Array.length r1) - 1 in
    let col_num_r2 = (Array.length r2) - 1 in
    let sum = ref 0. in
    let _ = if col_num_r1 == col_num_r2 then
        for c = 0 to (col_num_r1) do
          sum := !sum +. (r1.(c) *. r2.(c))
        done
      else
        failwith "Cannot take the dot product of two different-sized vectors"
    in !sum

  (**[make_dependent_variable r b] is the row of coefficients that express 
     the pivot of the row [r], set equal to the float [b]
     Requires: [r] must have a nonzero element*)
  let make_dependent_variable r b =
    let col_num = (Array.length r) - 1 in
    let dependent_variable = Array.create_float (col_num+1) in
    let p = pivot_pos r in
    dependent_variable.(p) <- 1.;
    for c = (p+1) to col_num do
      dependent_variable.(c) <- (-1. *. r.(c))
    done;
    dependent_variable

  let base_matrix v = 
    let aug = List.nth v 0 |> unwrap_matrix in
    let row_num = (Array.length aug) - 1 in
    let col_num = Array.length (Array.get aug 0) - 1 in
    let a = make_matrix_helper (row_num+1) (col_num) in
    for r = 0 to row_num do
      for c = 0 to col_num-1 do
        a.(r).(c) <- aug.(r).(c)
      done
    done; VMatrix a
  (**[one_solution_helper aug] is one particular solution to the equation
     Ax=b given by the augmented matrix [aug]. If only one discrete solution
     exists, then it is returned. If a continuous solution (many solutions)
     exist, then one such possible solution is returned by having every free
     variable in the solution equal to 1.0.
     Requires [aug] to be a matrix of more than one column.*)
  let one_solution_helper aug =
    let col_num_aug = Array.length (Array.get aug 0) - 1 in
    let b = 
      nth_col [VMatrix aug; VFloat (float_of_int col_num_aug)] 
      |> unwrap_matrix
    in
    let a = base_matrix [VMatrix aug] |> unwrap_matrix in
    let row_num = (Array.length a) - 1 in
    let col_num = Array.length (Array.get a 0) - 1 in
    let dependent_variables = make_matrix_helper (col_num+1) (col_num+1) in
    let solution_vector = Array.create_float (col_num+1) in
    for r = 0 to row_num do
      solution_vector.(r) <- b.(r).(0)
    done;
    let _  = 
      for r = 0 to col_num do
        if r > (row_num) || (pivot_pos a.(r) == -1) then 
          (*Free Variable at r*)
          let _ = solution_vector.(r) <- 1. in
          dependent_variables.(r).(r) <- 1.;
        else 
        if pivot_pos a.(r) == -1 && ((fuzzy_compare b.(r).(0) 0.)=1) then 
          failwith "No solution is possible" (*No Solution*)
        else (*Dependent Variable at r*)
          dependent_variables.(r) <- make_dependent_variable a.(r) b.(r).(0)
      done in
    let _ = 
      for r = 0 to row_num do (*Multiply dependent vars by independent vars*)
        solution_vector.(r) <- 
          (dot_product_row dependent_variables.(r) solution_vector);
      done
    in
    solution_vector

  let one_solution v =
    let a = List.nth v 0 |> unwrap_matrix in
    VRow (one_solution_helper a)

  let eigenvectors v =
    let m = List.nth v 0 |> unwrap_matrix in
    let n = (Array.length m) - 1 in
    let eigenvalues = List.nth v 1 in
    let eigenvalues_length = (eigenvalues |> unwrap_row |> Array.length) -1 in
    let eigenvalues_array = eigenvalues |> unwrap_row in
    let eigenvector_array = make_matrix_helper (n+1) (n+1) in
    let _ =
      for i = 0 to eigenvalues_length do
        let identity = 
          identity_matrix [(VFloat (float_of_int (n+1)))]  in
        let lambda = VFloat (eigenvalues_array.(i)) in
        let lambda_matrix = matrix_constant_multiply [identity; lambda] in
        let mcopy = VMatrix (Array.copy m) in
        let characteristic_matrix = sub_matrix [mcopy; lambda_matrix] in
        let zero_vector = make_matrix_helper (eigenvalues_length+1) 1 in
        let augmented_matrix = matrix_augment [
            characteristic_matrix;
            VMatrix zero_vector;
          ] in
        let solution_matrix = reduced_echelon_form [augmented_matrix] in
        let eigenvector = one_solution [solution_matrix] |> unwrap_row in
        for r = 0 to n do
          eigenvector_array.(r).(i) <- eigenvector.(r)
        done
      done
    in VMatrix eigenvector_array

end

module Matrix_CFU : CFU_sig = struct
  let operation_list = [
    ("madd", Matrix_Functions.add_matrix);
    ("msub", Matrix_Functions.sub_matrix);
    ("mdot", Matrix_Functions.dot_product_matrix);
    ("echelon", Matrix_Functions.echelon_form);
    ("rref", Matrix_Functions.reduced_echelon_form);
    ("determinant", Matrix_Functions.determinant);
    ("transpose", Matrix_Functions.transpose);
    ("midentity", Matrix_Functions.identity_matrix);
    ("mmult", Matrix_Functions.matrix_constant_multiply);
    ("nthcol", Matrix_Functions.nth_col);
    ("maugment", Matrix_Functions.matrix_augment);
    ("eigenvectors", Matrix_Functions.eigenvectors);
    ("onesolution", Matrix_Functions.one_solution);
    ("basematrix", Matrix_Functions.base_matrix);

  ]
end