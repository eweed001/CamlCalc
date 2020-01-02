open Stdlib


(** type for the doors *)
type door = Car | Goat

(** type for the game state *)
type game_state = 
  {
    choice : string;
    doors : door * door * door;
    item : door; 
    switched : string;
  }

(** [Error] is the error for a game play error *)
exception Error of string

(** [fst triple] is the first element of [triple] *)
let fst triple = 
  match triple with 
  |(h, h2, t) -> h


(** [snd triple] is the second element of [triple] *)
let snd triple = 
  match triple with 
  |(h,h2,t) -> h2


(** [thrd triple] is the third element of [triple] *)
let thrd triple = 
  match triple with 
  |(h,h2,t) -> t

(** [string_of_door door] is the string representation of [door] *)
let string_of_door door = 
  match door with 
  |Car -> "car"
  |Goat -> "goat"

(** [permutation] is a random permutation of 3 doors, 2 of type [Goat] and 
    one of type [Car]. *)
let permutation = 
  Random.self_init();
  let n = Random.int 3 in
  let d1 = [|Car; Goat; Goat|].(n) in 
  if d1 = Car then begin 
    let d2 = Goat in 
    let d3 = Goat in 
    (d1,d2,d3) end 
  else begin
    let d2 = [|Car; Goat|].(Random.int 2) in
    if d2 = Car then let d3 = Goat in (d1,d2,d3)
    else let d3 = Car in 
      (d1,d2,d3) end

(** [find_car triple] is the letter of the door with the car behind it *)
let find_car triple = 
  match triple with 
  |(Car,_,_)-> "A"
  |(_, Car,_)-> "B"
  |(_,_,Car)->"C"
  |_ -> failwith "no car"

(** [state_printer st] prints out [st] - used for debugging *)
let state_printer st = 
  match st with 
  |{choice = c; doors = d; item = i; switched = s} -> 
    print_string (c ^", doors perm, " ^ (string_of_door i) ^ ", " ^ s) 


(** [choice d perm] is the item ([Goat] or [Car]) behind the user's choice [d]
    in door permutation [perm] *)
let rec choice d perm = 
  match d with 
  |"A" -> fst perm
  |"B" -> snd perm
  |"C"-> thrd perm
  |_ -> print_string  "Invalid door choice, please try again"; 
    let () = start () in fst perm 

(** [play1 ch] is the [game_state] after the user has chosen door [ch]
    and another door has been opened accordingly *)
and play1 ch = 
  try(
    let perm = permutation in
    let fst_item = choice ch perm in 
    match ch with 
    |"A" as d -> if (fst_item = Goat) then (
        if ((snd perm) = Goat) then
          let () = print_string
              "I will open door B\nBehind door B is A Goat" in 
          {choice = d; doors = perm; item = fst_item; switched = "B"}
        else
          let () = print_string 
              "\nI will open door C\nBehind door C is a Goat" 
          in {choice = d; doors = perm; item = fst_item; switched = "C"})
      else (
        let x = Random.int 2 in 
        if x = 1 then 
          let () = print_string
              "I will open door B\nBehind door B is a Goat" in 
          {choice = d; doors = perm; item = fst_item; switched = "B"}
        else 
          let () = print_string 
              "\nI will open door C\nBehind door C is a Goat" 
          in {choice = d; doors = perm; item = fst_item; switched = "C"})

    |"B" as d -> if fst_item = Goat then begin
        if ((fst perm) = Goat) then 
          let () = print_string
              "I will open door A\nBehind door A is a Goat" in 
          {choice = d; doors = perm; item = fst_item; switched = "A"}
        else 
          let () = print_string 
              "\nI will open door C\nBehind door C is a Goat" 
          in {choice = d; doors = perm; item = fst_item; switched = "C"} end
      else begin
        let x = Random.int 2 in if x = 1 then 
          let () = print_string 
              "I will open door A\nBehind door A is a Goat" in 
          {choice = d; doors = perm; item = fst_item; switched = "A"}
        else 
          let () = print_string  
              "\nI will open door C\nBehind door C is a Goat" 
          in {choice = d; doors = perm; item = fst_item; switched = "C"}
      end
    |"C" as d -> if fst_item = Goat then begin
        if ((fst perm) = Goat) then 
          let () = print_string 
              "I will open door A\nBehind door A is a Goat" in 
          {choice = d; doors = perm; item = fst_item; switched = "A"}
        else 
          let () = print_string
              "\nI will open door B\nBehind door B is a Goat " 
          in {choice = d; doors = perm; item = fst_item; switched = "B"} end
      else begin
        let x = Random.int 2 in if x = 1 then 
          let () = print_string 
              "I will open door A\nBehind door A is a Goat" in 
          {choice = d; doors = perm; item = fst_item; switched = "A"}
        else 
          let () = print_string 
              "\nI will open door B\nBehind door B is a Goat" 
          in {choice = d; doors = perm; item = fst_item; switched = "B"}
      end
    | _ -> failwith "no"

  ) 
  with 
  |Error s -> 
    print_string  "An error has occured... please start over"; play1 ch


(** [play2 st] completes the game by giving the option  to switch doors to the 
    user and handling it accordingly and printing the item 
    behind the user's final door choice *)
and  play2 state = 
  let () = print_string "\nWould you like to switch doors? Yes or No" in 
  print_string  "\n>";
  let choice = String.trim (String.lowercase_ascii (read_line())) in 
  if choice = "no" then end_game state
  else if choice = "yes" then let state' = switch_doors state  in 
    end_game state'
  else let () = print_string "Not a valid command. Please try again" 
    in play2 state


(** [end_game st] prints out the result behind the final door choice
    in [ch] *)
and end_game st = 
  let () = print_string  ("You have chosen door " ^ st.choice ^"") in 
  if ((st.item) = Car) then begin
    print_string ("\nBehind door " ^ st.choice ^" is a Car!! 
    You have won. Congratulations!
     For an explanation of the probability behind this game,
    please type \"monty hall explanation\"\n"); end
  else let winning_door = find_car (st.doors) in 
    let () = print_string
        ("\nBehind door " ^ st.choice ^ " is a Goat. The car was behind door " 
         ^winning_door ^ ". You have lost, better luck next time!
         For an explanation of the probability behind this game, please type 
         \"monty hall explanation\"\n") in ()

(** [switch_doors st] updates [st] according to which door the user
    could have switched to  *)
and switch_doors st = 
  let opened = st.switched in
  let fst_choice = st.choice in  
  if (opened = "A" && fst_choice = "B") then 
    {choice = "C"; doors = st.doors; item = thrd st.doors; switched = opened}
  else if (opened = "A" && fst_choice = "C") then
    {choice = "B"; doors = st.doors; item = snd st.doors; switched = opened}
  else if (opened = "B" && fst_choice = "A") then 
    {choice = "C"; doors = st.doors; item = thrd st.doors; switched = opened}
  else if (opened = "B" && fst_choice = "C") then 
    {choice = "A"; doors = st.doors; item = fst st.doors; switched = opened}
  else if (opened = "C" && fst_choice = "A") then
    {choice = "B"; doors = st.doors; item = snd st.doors; switched = opened}
  else if (opened = "C" && fst_choice = "B") then 
    {choice = "A"; doors = st.doors; item = fst st.doors; switched = opened}
  else failwith "shouldnt happen"


and start () =
  let st = Random.State.make_self_init() in 
  Random.set_state st;
  let () = Random.self_init() in
  print_string 
    "Welcome to the game. There are 3 doors, A, B, C. Please choose one";
  print_string "\n>";
  let choice = String.trim (String.uppercase_ascii (read_line())) in
  let v = play1 choice in 
  let ()  = play2 v in () 
