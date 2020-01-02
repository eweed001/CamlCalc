open Ast

(** An abstract module type that is meant to structure the CFU modules 
    (ie. Aritmetic functions, calculus functions, statistics functions). 
    A module that matches [CFU_sig] is suitable for use in the [Imports] 
    module. *)
module type CFU_sig = sig
  (** An [operation_list] is an association list that maps operation 
      symbols to functions *)
  val operation_list : (string * ( value list -> value)) list
end

module type Matrix_Funcs = sig

  (** [make_matrix v]  is a matrix with x dimension as the first element of [v]
      and y dimension as the second element of [v] 
      Requires: [v] is of length = 2 and contains float values *)
  val make_matrix : value list -> value

  (** [add_matrix v] is a matrix that is the result of adding the first element
      of [v] and the second element of [v] 
      Requires: [v] is of length = 2 and contains elements of matrix type *)
  val add_matrix : value list -> value

  (** [sub_matrix v] is a matrix that is the result of subtracting the second 
      element of [v] from the first element of [v] 
      Requires: [v] is of length = 2 and contains elements of matrix type *)
  val sub_matrix : value list -> value

  (**[dot_product_matrix v] is a matrix that is the scalar product of the first 
     and second elements of [v] 
     Requires: [v] is of length = 2, contains elements of matrix type and 
     matrices of dimensions (mxn) and (nxr) where m,n,r are positive integers *)
  val dot_product_matrix : value list -> value

  (** [echelon_form v] is a matrix that is equivalent to the echelon form of the
      first element of [v]
      Requires: [v] is of length = 1 and contains element of matrix type *)
  val echelon_form : value list -> value

  (** [reduced_echelon_form v] is a matrix that is equivalent to the reduced 
      echelon form of the first element of [v]
        Requires: [v] is of length = 1 and contains element of matrix type *)
  val reduced_echelon_form : value list -> value

  (** [determinant v] is a matrix that is equivalent to determinant of the 
      matrix that is the first element of [v]
        Requires: [v] is of length = 1 and contains element of matrix type *)
  val determinant : value list -> value

  (** [transpose v] is the transpose of a matrix given by the first element of
       [v]
       Requires: The first element of v is a [VMatrix] *)
  val transpose : value list -> value

  (** [identity_matrix v] is the identity matrix given by the first with rows
       and columns equal to the first element of [v]
       Requires: The first element of v is a [VFloat] *)
  val identity_matrix : value list -> value

  (** [matrix_constant_multiply v] is the matrix given by the 
       multiplication of the matrix at the first element of [v] by the float at
       the second element of [v].
       Requires: The first element of v is a [VMatrix]
       Requires: The second element of v is a [VFloat] *)
  val matrix_constant_multiply : value list -> value

  (** [matrix_augment v] is the matrix given by the 
       augmentation of the matrix at the first element of [v] by the matrix at
       the second element of [v].
       Requires: The first element of v is a [VMatrix]
       Requires: The second element of v is a [VMatrix] column vector
       with 1 column and n rows, where n is equal to the number of rows of the 
       matrix at first element of v *)
  val matrix_augment : value list -> value

  (** [nth_col v] is the matrix column vector of the matrix of the first 
       element of [v] at the column number at the second element of [v].
       Requires: The first element of v is a [VMatrix]
       Requires: The second element of v is a [VFloat] *)
  val nth_col : value list -> value

  (** [eigenvectors v] is the matrix where each column is equal to an 
       eigenvector of the matrix at the first element of [v], corresponding to 
       the eigenvalue of the row of eigenvalues at the second element of [v]
       Requires: The first element of v is a [VMatrix]
       Requires: The second element of v is a [VRow] *)
  val eigenvectors : value list -> value

  (** [one_solution v] is the matrix column vector of the solution to the
       equation Ax=b, where Ab is the augmented matrix at the first element 
       of [v]. Free variables are set to 1, and dependent variables are
       defined as a linear combination of free variables.
       Requires: The first element of v is a [VMatrix] *)
  val one_solution : value list -> value

  (** [base_matrix v] is the matrix base matrix in the augmented matrix given by
       the first element of [v], corresponding to A in the equation
       Ax=b where Ab is the augmented matrix
       Requires: The first element of v is a valid augmented matrix of more 
       than one column and is a [VMatrix] *)
  val base_matrix : value list -> value
end

(** [Matrix_Functions] is a module that implements the function values defined 
    in [Matrix_Funcs]. Contains all the matrix functionalities in CAMLCALC *)
module Matrix_Functions : Matrix_Funcs

(** [Matrix_CFU] is a module of type [CFU_sig] which contains the 
    [operation_list] with matrix functions *)
module Matrix_CFU : CFU_sig