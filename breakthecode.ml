
open Stdlib
open Printf


(** [Error] is the error for a game play error *)
exception Error of string


(**  [perm] generates the random 5 number code *)
let perm = 
  Random.self_init();
  let n1 = Random.int 10 in 
  let n2 = Random.int 10 in 
  let n3 = Random.int 10 in 
  let n4 = Random.int 10 in 
  let n5 = Random.int 10 in 
  let list = n1 :: n2 :: n3 :: n4 :: n5 :: [] in 
  list 


(** [end_game_lose perm] is the end game message to print out when the user
    has used all their chances and has not correctly guess the code *)
let end_game_lose perm = 
  print_endline "You have used all your chances... 
  The correct code was: "; 
  List.iter (printf "%d ") perm; print_endline""; ()


(** [my_compare guess perm] is the result of checking if [guess] and [perm]
    are equal *)
let my_compare guess perm = 
  let str = List.fold_left (fun acc i -> acc^(string_of_int i)) "" perm in 
  str = guess


(** [hint_printer guess perm num] prints out the hint string according to the 
    user's most recent guess according to the rules defined, handling 
    invalid guesses *)
let rec hint_printer guess perm guessnumber =
  try (
    let s1 = if (string_of_int(List.nth perm 0) = String.sub guess 0 1) then "."
      else if (List.mem (int_of_string (String.sub guess 0 1)) perm) then "*" 
      else "-" in 
    let s2 = if (string_of_int(List.nth perm 1) = String.sub guess 1 1) then "."
      else if (List.mem (int_of_string(String.sub guess 1 1)) perm) then "*" 
      else "-" in 
    let s3 = if (string_of_int(List.nth perm 2) = String.sub guess 2 1) then "."
      else if (List.mem (int_of_string(String.sub guess 2 1)) perm) then "*" 
      else "-" in 
    let s4 = if (string_of_int(List.nth perm 3) = String.sub guess 3 1) then "."
      else if (List.mem (int_of_string(String.sub guess 3 1)) perm) then "*" 
      else "-" in 
    let s5 = if (string_of_int(List.nth perm 4) = String.sub guess 4 1) then "."
      else if (List.mem (int_of_string(String.sub guess 4 1)) perm) then "*" 
      else "-" in 
    print_endline (s1^s2^s3^s4^s5); ()
  ) with 
  |Failure f ->
    print_string "Invalid guess, please try again";
    print_string "\n>";
    let guess = String.trim (String.uppercase_ascii (read_line())) in
    game_play guess perm guessnumber


(** [game_play guess perm num] prints out the result of the user playing the 
    Break the Code game *)
and game_play guess perm guessnumber = 
  try(
    match guessnumber with 
    |16 -> end_game_lose perm
    |i -> let bool = my_compare guess perm in 
      if bool then print_endline "Congratulations, You have won!! " 
      else let () = hint_printer guess perm guessnumber in 
        print_string "\n>";
        let guess' = String.trim (String.uppercase_ascii (read_line())) in 
        game_play guess' perm (guessnumber + 1)
  )
  with 
  |Error s -> 
    print_string "Invalid guess, please try again";
    print_string  "\n>";
    let guess = String.trim (String.uppercase_ascii (read_line())) in
    game_play guess perm guessnumber



let start () = 
  let () =  print_endline " Welcome to the Break the Code game. 
A random sequence of 5 numbers (0-9) will be generated and you will get 15 
guesses to get it correct. 
Hints will be printed out after each guess in the form of:
. -> a correct number in the correct position 
- -> a incorrect number
* -> a correct number in the wrong position
Good Luck!" in 
  let () = print_endline "Guess #1 : " in 
  print_string  "\n>";
  let guess = String.trim (String.uppercase_ascii (read_line())) in
  game_play guess perm 1

