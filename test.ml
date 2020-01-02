open OUnit2
open Ast
open Main
(** Test Plan: For our test cases, we used OUnit to test most of our 
      mathematical functions in our CFUs and we used OUnit to test language 
      features. However, there were some parts of our calculator that we could
      not test with OUnit, which we tested interactively due to these being 
      difficult to develop test cases for. These parts that we tested 
      interactively were the graphing and user defined functions. 
      The OUnit tests we wrote were developed using black box testing and 
      we made sure to cover all edge cases. Our testing plan proves our system 
      correctness because by passing these test cases, it proves all of our 
      mathematical functions in our CFU's are correct, and it also proves our 
      calculator language features are correct. For user defined functions and 
      graphing, we tested these interactively. We had to do so 
      because the graphing function we wrote output graphs to an image file. Our 
      interactive testing proves our graphing is correct because we compared 
      the graphs that we generated to graphs we generated on other graphing
      calculators and they looked exactly the same.
      Overall, our testing plan proves the correctness of our system 
      because it makes sure every small part of it works as intended. *)

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Float i]. *)
let make_i n i s =
  n >:: (fun _ -> 
      assert_equal i (fst (Main.interp s Main.initial_env)) 
        ~printer:(fun x-> x))

let tests = [
  make_i "float" "22." "22";
  make_i "add" "22." "11+11";
  make_i "mul1" "22." "2*11";
  make_i "mul2" "22." "2+2*10";
  make_i "mul3" "14." "2*2+10";
  make_i "mul4" "40." "2*2*10";
  make_i "let_definition_simple" "5." "let x = 5";
  make_i "let_expression" "0." "let x = 5 in 0";
  make_i "let_expression_complex" "4." "let a = let b = 4 in b in a";
  make_i "let_expression_complex2" "4."
    "let a = let b = 4 in b in let b = a in b";
  make_i "if_expression1" "1." "if true then 1 else 2";
  make_i "if_expression1" "2." "if false then 1 else 2";
  make_i "if_expression1" "1." "if true then if false then 2 else 1 else 2";
  make_i "boolean" "true" "true";
  make_i "div" "4." "16 / 4";
  make_i "modulus" "6." "13 % 7";
  make_i "modulus" "0." "30 % 3";
  make_i "exponent" "25." "5^2";
  make_i "exponent" "9." "9^1";
  make_i "exponent" "4." "16^0.5";
  make_i "log" "3." "8 log 2";
  make_i "log" "2." "100 log 10";
  make_i "sin" "0." "sin 0";
  make_i "sin" "1." "sin 90";
  make_i "sin" "0.5" "sin 30";
  make_i "cos" "1." "cos 0";
  make_i "cos" "0." "cos 90";
  make_i "cos" "0.5" "cos 60";
  make_i "tan" "0." "tan 0";
  make_i "tan" "1." "tan 45";
  make_i "tan " "0." "tan 180";
  make_i "csc" "2." "csc 30";
  make_i "csc" "1." "csc 90";
  make_i "sec" "1." "sec 0";
  make_i "sec" "2." "sec 60";
  make_i "cot" "1." "cot 45";
  make_i "cot" "0." "cot 90";

  make_i "if then else" "true" "if 1 then true else false";
  make_i "if then else" "false" "if 0 then true else false";
  make_i "if then else" "true" "if 1==1 then true else false";
  make_i "if then else" "false" "if 1==9 then true else false";

  make_i "tan - undefined" "undefined" "tan 90";
  make_i "tan - undefined" "undefined" "tan 270";
  make_i "csc - undefined" "undefined" "csc 0";
  make_i "csc - undefined" "undefined" "csc 180";
  make_i "sec - undefined" "undefined" "sec 90";
  make_i "sec - undefined" "undefined" "sec 270";
  make_i "cot - undefined" "undefined" "cot 0";
  make_i "cot - undefined" "undefined" "cot 180";
  make_i "stats - mean" "3." "let set = [1 2 3 4 5] ; mean set";
  make_i "stats - mean" "1." "let set = [1] ; mean set ";
  make_i "stats - mean" "2." "let set = [2 2] ; mean set";
  make_i "stats - mean" "1.5" "let set = [3 0] ; mean set";
  make_i "stats - median" "3." "let set = [1 2 3 4 5] ; median set";
  make_i "stats - median" "3." "let set = [2 4 5 1 3] ; median set";
  make_i "stats - median" "3.5" "let set = [1 2 3 4 5 6] ; median set";
  make_i "stats - median" "3.5" "let set = [6 3 4 2 1 5] ; median set";
  make_i "stats - median" "3." "let set = [3] ; median set";
  make_i "stats - max" "70." "let set = [0 1 2 3 4 70] ; max set";
  make_i "stats - max" "8." "let set = [8 3 2] ; max set";
  make_i "stats - max" "5." "let set = [5] ; max set";
  make_i "stats - min" "5." "let set = [5] ; min set";
  make_i "stats - min" "1." "let set = [1 2 3 4 5] ; min set";
  make_i "stats - min" "0." "let set = [4 5 8 2 9 4 9000 0] ; min set";
  make_i "stats - stddev" "2." "let set = [1 5] ; stdev set";
  make_i "stats - stddev" "1." "let set = [2 4] ; stdev set";
  make_i "stats - stddev" "1." "let set = [2 2 4 4] ; stdev set ";

  make_i "sets - make_set1" "[ 1.]" "let row = [1]";
  make_i "sets - make_set2" "[]" "let row = []";
  make_i "sets - make_set3" "[ 0.]" "let row = [0]";
  make_i "sets - make_set4" "[ 1. 2. 3.]" "let row = [ 1 2 3]";
  make_i "sets - union1" "[ 1. 2. 3. 4. 5. 6.]" 
    "let rone = [1 2 3 4] ; let rtwo = [4 5 6] ; union rone rtwo";
  make_i "sets - union2" "[ 1. 2. 3. 4. 5. 6. 7.]" 
    "let rone = [1 2 3] ; let rtwo = [4 5 6 7] ; union rone rtwo";
  make_i "sets - union3" "[ 0.]" 
    "let rone = [0] ; let rtwo = [0] ; union rone rtwo";
  make_i "sets - union4" "[ 200. 500. 900.]" 
    "let rone = [900 200 500] ; let rtwo = [500 200 900] ; union rone rtwo";
  make_i "sets - intersect1" "[ 900. 200. 500.]" 
    "let rone = [900 200 500] ; let rtwo = [500 200 900] ; intersect rone rtwo";
  make_i "sets - intersect2" "[]" 
    "let rone = [1] ; let rtwo = [0] ; intersect rone rtwo";
  make_i "sets - intersect3" "[]" 
    "let rone = [900 200 500] ; let rtwo = [1 2 3 4] ; intersect rone rtwo";
  make_i "sets - intersect4" "[ 4. 3. 2. 1.]" 
    ("let rone = [1 2 3 4 6 7 8] ; let rtwo = [1 2 3 4 5 9] ; "^
     " intersect rone rtwo");
  make_i "sets - intersect5" "[ 6. 8. 1. 2.]" 
    "let rone = [2 3 1 5 6 8];let rtwo = [4 2 9 1 7 8 6] ; intersect rone rtwo";
  make_i "sets - intersect6" "[]" 
    "let rone = [] ; let rtwo = [] ; intersect rone rtwo";
  make_i "sets - is_equal1" "true" 
    "let rone = [1 2 3 4]; let rtwo = [1 2 3 4]; is_equal rone rtwo";
  make_i "sets - is_equal2" "false" 
    "let rone = [1 2 3 4 5 6]; let rtwo = [1 2 3 4]; is_equal rone rtwo";
  make_i "sets - is_equal3" "false" 
    "let rone = [1 2 3 4]; let rtwo = [1 2 3 4 5 6]; is_equal rone rtwo";
  make_i "sets - is_equal4" "false" 
    "let rone = [1]; let rtwo = [0]; is_equal rone rtwo";
  make_i "sets - is_equal5" "true" 
    "let rone = [0]; let rtwo = [0]; is_equal rone rtwo";
  make_i "sets - is_equal6" "true" 
    "let rone = [1]; let rtwo = [1]; is_equal rone rtwo";
  make_i "sets - is_equal7" "true" 
    "let rone = [1 2 3 4]; let rtwo = [4 3 2 1]; is_equal rone rtwo";
  make_i "sets - is_equal8" "true" 
    "let rone = [900 500 200]; let rtwo = [200 900 500]; is_equal rone rtwo";
  make_i "sets - is_equal9" "false" 
    ("let rone = [900 1000 700]; "^
     "let rtwo = [50 900 1000 700]; is_equal rone rtwo");
  make_i "sets = is_disjoint1" "true" 
    "let rone = [1]; let rtwo = [0]; is_disjoint rone rtwo";
  make_i "sets = is_disjoint2" "false" 
    "let rone = [1]; let rtwo = [0 1 2]; is_disjoint rone rtwo";
  make_i "sets = is_disjoint3" "false" 
    "let rone = [1 2 3 4]; let rtwo = [4 5 6 7]; is_disjoint rone rtwo";
  make_i "sets = is_disjoint4" "false" 
    "let rone = [1 2 3 4 900]; let rtwo = [5 6 7 900]; is_disjoint rone rtwo";
  make_i "sets = is_disjoint5" "true" 
    "let rone = [1 2 3 4]; let rtwo = [5 6 7 8]; is_disjoint rone rtwo";
  make_i "sets = is_disjoint6" "false" 
    "let rone = [0]; let rtwo = [0]; is_disjoint rone rtwo";
  make_i "sets = is_disjoint7" "true" 
    "let rone = [50 100 150]; let rtwo = [51 101 151]; is_disjoint rone rtwo";
  make_i "sets = is_disjoint8" "false" 
    "let rone = [100 200 300]; let rtwo = [300 200 100]; is_disjoint rone rtwo";
  make_i "sets = is_subset1" "true"
    "let rone = [100 200 300]; let rtwo = [300 200 100]; is_subset rone rtwo";
  make_i "sets = is_subset2" "true"
    "let rone = [0]; let rtwo = [0 1]; is_subset rone rtwo";
  make_i "sets = is_subset3" "false"
    "let rone = [0]; let rtwo = [1]; is_subset rone rtwo";
  make_i "sets = is_subset4" "true" 
    "let rone = [1 2 3 4]; let rtwo = [1 2 3 4 5 6 7 8]; is_subset rone rtwo";
  make_i "sets = is_subset5" "false"
    "let rone = [1]; let rtwo = [100 200 300]; is_subset rone rtwo";
  make_i "sets = is_subset6" "false"
    "let rone = [600 900 300]; let rtwo = [601 301 901]; is_subset rone rtwo";
  make_i "sets = is_subset7" "true"
    "let rone = [0]; let rtwo = [0 1]; is_subset rone rtwo";
  make_i "sets = is_subset8" "false"
    "let rone = [3]; let rtwo = [0 1]; is_subset rone rtwo";
  make_i "sets = length1" "1." "let rone = [0] ; length rone";
  make_i "sets = length2" "2." "let rone = [0 1] ; length rone";
  make_i "sets = length3" "2." "let rone = [100 200] ; length rone";
  make_i "sets = length4" "10." 
    "let rone = [0 1 2 3 4 5 6 7 8 9] ; length rone";
  make_i "sets = length5" "4." "let rone = [100 201 302 403] ; length rone";
  make_i "sets = length6" "15." 
    "let rone = [3 10 9 4 5 2 7 15 11 12 20 76 54 17 18] ; length rone";

  make_i "sets - length5" "4." "let rone = [100 201 302 403] ; length rone";
  make_i "sets - length6" "15." 
    "let rone = [3 10 9 4 5 2 7 15 11 12 20 76 54 17 18] ; length rone";
  make_i "sets - append1" "[ 1. 2. 3. 4. 5.]" "let s = [1 2 3 4] ; append s 5";
  make_i "sets - append2" "[ 2. 4. 6. 8. 10.]" 
    "let s = [2 4 6 8] in append s 10";
  make_i "sets - append3" "[ 1.]" "let s = [] in append s 1";
  make_i "sets - append4" "[ 100. 200. 250. 300. 350. 400.]" 
    "let s = [100 200 250 300 350] in append s 400";
  make_i "sets - append5" "[ 100. 200. 250. 300. 350. 400. 200.]" 
    "let s = [100 200 250 300 350 400] in append s 200";
  make_i "sets - append6" "[ 1. 0.]" "let s = [1] in append s 0";
  make_i "sets - append7" "[ 9000. 8000. 2000.]" 
    "let s = [9000 8000] in append s 2000";
  make_i "sets - remove1" "[ 1. 2. 4. 5.]" 
    "let s = [1 2 3 4 5] in remove_val s 3";
  make_i "sets - remove2" "[ 100. 200. 300. 400.]" 
    "let s = [100 200 300 350 400] in remove_val s 350";
  make_i "sets - remove3" "[ 7. 8. 5. 4. 12. 10.]" 
    "let s = [ 7 8 5 4 12 10] in remove_val s 100000";
  make_i "sets - remove4" "[]" 
    "let s = [1] in remove_val s 1";
  make_i "sets - remove5" "[]" 
    "let s = [] in remove_val s 100000";
  make_i "sets - remove6" "[ 1. 600. 800. 750. 100.]" 
    "let s = [1 600 800 750 100] in remove_val s 650";
  make_i "sets - get_elem1" "3." "let s = [1 2 3 4 5] in get_elem s 2";
  make_i "sets - get_elem2" "1." "let s = [1 2 3 4 5] in get_elem s 0";
  make_i "sets - get_elem3" "Please enter a valid index" 
    "let s = [] in get_elem s 0";
  make_i "sets - get_elem4" "Please enter a valid index" 
    "let s = [] in get_elem s 100";
  make_i "sets - get_elem5" "Please enter a valid index" 
    "let s = [100 200 500 600] in get_elem s 100";
  make_i "sets - get_elem6" "10000." 
    ("let s = [1000 2000 3000 4000 5000 6000 7000 8000 9000 10000]" 
     ^ "in get_elem s 9");

  make_i "matrices - make matrix" "[|1., 2.|\n |3., 4.|]" 
    "let matr = matrix [1 2] [3 4]";
  make_i 
    "matrices - make matrix" "[|1., 2., 3.|\n |3., 4., 5.|\n |5., 6., 7.|]" 
    "let matr = matrix [1 2 3] [3 4 5] [5 6 7]";
  make_i "matrices - make matrix" "[|1.|]" "let matr = matrix [1]";

  make_i "matrices - addition1" "[|2., 4.|\n |6., 8.|]"
    "let matr = matrix [1 2][3 4] in madd matr matr";
  make_i "matrices - addition2" "[|6., 8.|\n |10., 12.|]"
    ("let matr = matrix [1 2] [3 4] in "^ 
     "let mtwo = matrix [5 6] [7 8] in "^ 
     "let m = madd matr mtwo in m");
  make_i "matrices - addition3" "[|1., 2.|\n |3., 4.|]"
    ("let matr = matrix [1 2] [3 4] in "^
     "let mtwo = matrix [0 0] [0 0 ] in let m = madd matr mtwo in m"); 
  make_i "matrices - addition4" "[|8., 10.|\n |12., 14.|\n |16., 18.|]"
    ("let matr = matrix [1 2] [3 4] [5 6] in "^
     "let mtwo = matrix [7 8] [9 10] [11 12] in let m = madd matr mtwo in m");
  make_i "matrices - addition5" "[|8., 10., 12.|\n |12., 14., 16.|]"
    ("let matr = matrix [1 2 3] [3 4 5] in "^
     "let mtwo = matrix [7 8 9] [9 10 11] in let m = madd matr mtwo in m"); 
  make_i "matrices - addition failwith" 
    "Invalid input - matrices must be the same size"
    ("let matr = matrix [1 2 ] [3 4 ] in "^
     "let mtwo = matrix [7 8 9] [9 10 11] in let m = madd matr mtwo in m");


  make_i "matrices - subtraction1" "[|0., 0.|\n |0., 0.|]"
    "let matr = matrix [1 2] [3 4] in let m = msub matr matr in m"; 
  make_i "matrices - subtraction2" "[|1., 1.|\n |2., 2.|]"
    ("let matr = matrix [1 2] [3 4] in "^
     "let mtwo = matrix [0 1] [1 2] in let m = msub matr mtwo in m"); 
  make_i "matrices - subtraction3" "[|-4., -7.|\n |-1., -4.|]"
    ("let matr = matrix [1 2] [3 4] in "^
     "let mtwo = matrix [5 9] [4 8] in let m = msub matr mtwo in m"); 
  make_i "matrices - subtraction4" "[|1., -7.|\n |-1., -4.|\n |2., 4.|]"
    ("let matr = matrix [1 2] [3 4] [5 6] in "^
     "let mtwo = matrix [0 9] [4 8] [3 2] in let m = msub matr mtwo in m");
  make_i "matrices - subtraction failwith" 
    "Invalid input - matrices must be the same size"
    ("let matr = matrix [1 2 ] [3 4 ] in "^
     "let mtwo = matrix [7 8 9] [9 10 11] in let m = msub matr mtwo in m");

  make_i "matrices - mdot1" "[|1., 0.|\n |0., 1.|]"
    "let matr = matrix [1 0] [0 1] in let m = mdot matr matr in m";
  make_i "matrices - mdot2" "[|7., 10.|\n |15., 22.|]"
    "let matr = matrix [1 2] [3 4] in let m = mdot matr matr in m";
  make_i "matrices - mdot3" "[|25., 14.|\n |57., 36.|\n |89., 58.|]"
    ("let matr = matrix [1 2] [3 4] [5 6] in "^
     "let mtwo = matrix [7 8] [9 3] in let m = mdot matr mtwo in m");
  make_i "matrices - mdot4" 
    "[|43., 26., 16.|\n |93., 60., 40.|\n |143., 94., 64.|]"
    ("let matr = matrix [1 2 2] [3 4 4] [5 6 6] in "^
     "let mtwo = matrix [7 8 8] [9 3 3] [9 6 1] in "^
     "let m = mdot matr mtwo in m");
  make_i "matrices - mdot5" "[|7., 8.|\n |21., 24.|]"
    ("let matr = matrix [1 2] [3 4] in "^
     "let mtwo = matrix [7 8] [0 0] in let m = mdot matr mtwo in m");
  make_i "matrices - dot product failwith" 
    "Please make sure the matrices follow dimensions M x N, N x R"
    ("let matr = matrix [1 0] [0 1] in "^
     "let mtwo = matrix [1 2] [4 5] [6 7]  in let m = mdot matr mtwo in m");


  make_i "matrices - ref1" "[|1., 3.|\n |0., 1.|]"
    "let matr = matrix [1 3] [4 2] in let m = echelon matr in m";
  make_i "matrices - ref2" "[|1., 0.|\n |0., 1.|]"
    "let matr = matrix [1 0] [0 1] in let m = echelon matr in m";
  make_i "matrices - ref3" "[|1., 2.|\n |0., 1.|]"
    "let matr = matrix [4 8] [4 2] in let m = echelon matr in m";
  make_i "matrices - ref4" "[|1., 2.|\n |0., 1.|\n |0., 0.|]"
    "let matr = matrix [1 2] [3 4] [5 6] in let m = echelon matr in m";
  make_i "matrices - ref5" "[|1., 7.|\n |0., 1.|\n |0., 0.|]"
    "let matr = matrix [1 7] [3 1] [4 0] in let m = echelon matr in m";
  make_i "matrices - ref6" "[|1., 2., 3.|\n |0., 1., 2.|]"
    "let matr = matrix [1 2 3] [4 5 6] in let m = echelon matr in m";
  make_i "matrices - ref7" "[|1.|]"
    "let matr = matrix [1] in let m = echelon matr in m";

  make_i "matrices - rref1" "[|1., 0.|\n |0., 1.|]"
    "let matr = matrix [1 3] [4 2] in let m = rref matr in m";
  make_i "matrices - rref2" "[|1., 0.|\n |0., 1.|]"
    "let matr = matrix [1 0] [0 1] in let m = rref matr in m";
  make_i "matrices - rref3" "[|1., 0.|\n |0., 1.|]"
    "let matr = matrix [4 8] [4 2] in let m = rref matr in m";
  make_i "matrices - rref4" "[|1., 0.|\n |0., 1.|\n |0., 0.|]"
    "let matr = matrix [1 2] [3 4] [5 6] in let m = rref matr in m";
  make_i "matrices - rref5" "[|1., 0.|\n |0., 1.|\n |0., 0.|]"
    "let matr = matrix [1 7] [3 1] [4 0] in let m = rref matr in m";
  make_i "matrices - rref6" "[|1., 0., -1.|\n |0., 1., 2.|]"
    "let matr = matrix [1 2 3] [4 5 6] in let m = rref matr in m";
  make_i "matrices - rref7" "[|1.|]"
    "let matr = matrix [1] in let m = rref matr in m";

  make_i "derivatives" "10.01" "let f = fun (x) -> x*x in deriv f 5 0.01";
  make_i "derivatives2" "0.01" "let f = fun (x) -> x*x in deriv f 0 0.01";
  make_i "derivatives3" "2." "let f = fun (x) -> 2*x in deriv f 9 0.01";
  make_i "derivatives4" "0." "let f = fun (x) -> 2 in deriv f 4 0.01";
  make_i "derivatives5" "16.01" 
    "let f = fun (x) -> x*x+2*x+3 in deriv f 7 0.01";
  make_i "derivatives6" "12.0601" 
    "let f = fun (x) -> (x*x*x)+5 in deriv f 2 0.01";
  make_i "derivatives7" "0." 
    "let f = fun (x) -> 0*x in deriv f 100 0.01";

  make_i "integral" "330.68" "let f = fun (x) -> x*x in integ f 2 10";
  make_i "integral" "24." "let f = fun (x) -> 2*x in integ f 1 5";
  make_i "integral" "3.7575" "let f = fun (x) -> x*x*x in integ f 1 2";
  make_i "integral" "0." "let f = fun (x) -> x*0 in integ f 1 2";
  make_i "integral" "6." "let f = fun (x) -> 6 in integ f 1 2";
  make_i "integral" "23750." "let f = fun (x) -> x in integ f 450 500";

  make_i "eigenvector" "[|-1., -0.5|\n |1., 1.|]" 
    "let a = matrix [0 1] [-2 -3] in let eig = [-1 -2] in eigenvectors a eig ";
  make_i "eigenvector2" "[|1., 1.|\n |1., 1.|]" 
    "let m = midentity 2 in let eig = [1 1] in eigenvectors m eig"


]

let suite = "calc test suite" >::: tests

let _ = run_test_tt_main suite