open Poker

let show_card c =
  let r = match c.rank with
    | Two->"2"|Three->"3"|Four->"4"|Five->"5"|Six->"6"|Seven->"7"|Eight->"8"
    | Nine->"9"|Ten->"T"|Jack->"J"|Queen->"Q"|King->"K"|Ace->"A" in
  let s = match c.suit with Clubs->"♣"|Diamonds->"♦"|Hearts->"♥"|Spades->"♠" in
  r ^ s

let show_hand h = String.concat " " (List.map show_card h)
let show_board b = if b = [] then "(no board yet)" else show_hand b

let clear_screen () =
  print_string "\027[2J\027[H"; flush stdout

let press_enter msg =
  Printf.printf "%s" msg; flush stdout;
  ignore (read_line ())

let player_name p p1_name p2_name =
  match p with P1 -> p1_name | P2 -> p2_name

let prompt_action st =
  if st.to_call = 0 then
    print_string "Action (check/bet N/fold): "
  else
    Printf.printf "Facing bet of %d. Action (call/fold): " st.to_call

let parse_action line to_call =
  let line = String.lowercase_ascii (String.trim line) in
  match line with
  | "fold" -> Some Fold
  | "check" -> if to_call = 0 then Some Check else None
  | "call" -> if to_call > 0 then Some Call else None
  | _ ->
      if String.length line >= 3 && String.sub line 0 3 = "bet" && to_call = 0 then
        let n_str = String.trim (String.sub line 3 (String.length line - 3)) in
        (match int_of_string_opt n_str with
         | Some n when n > 0 -> Some (Bet n)
         | _ -> None)
      else None

let current_player_hand st =
  match st.decision with
  | In_progress { whose_turn = P1 } -> st.p1
  | In_progress { whose_turn = P2 } -> st.p2
  | _ -> []

let rec turn_loop st p1_name p2_name =
  (* Auto-deal if needed *)
  let st =
    match st.stage with
    | Predeal -> (match make_move st Check with Ok s -> s | Error _ -> st)
    | _ -> st
  in

  (* Terminal handling *)
  begin match st.stage with
  | Showdown ->
      let st = match make_move st Check with Ok s -> s | Error _ -> st in
      turn_loop st p1_name p2_name
  | Finished ->
      clear_screen ();
      Printf.printf "=== FINAL ===\nBoard: %s\n\n" (show_board st.board);
      Printf.printf "%s (P1): %s\n" p1_name (show_hand st.p1);
      Printf.printf "%s (P2): %s\n" p2_name (show_hand st.p2);
      (match st.decision with
       | Winner P1 -> Printf.printf "\n%s wins! Pot=%d\n" p1_name st.pot
       | Winner P2 -> Printf.printf "\n%s wins! Pot=%d\n" p2_name st.pot
       | _ -> Printf.printf "\nStalemate. Pot=%d\n" st.pot);
      flush stdout
  | _ ->
      (* Normal turn *)
      let who =
        match st.decision with
        | In_progress { whose_turn } -> whose_turn
        | _ -> P1
      in
      let pname = player_name who p1_name p2_name in

      clear_screen ();
      Printf.printf "%s's turn\n" pname;
      Printf.printf "Board: %s\nPot: %d   To_call: %d\n\n"
        (show_board st.board) st.pot st.to_call;

      let hand = current_player_hand st in
      Printf.printf "Your hand: %s\n\n" (show_hand hand);

      prompt_action st; flush stdout;
      let mv =
        let rec read_valid () =
          let line = read_line () in
          match parse_action line st.to_call with
          | Some mv -> mv
          | None ->
              print_endline "Invalid action for this situation. Try again.";
              prompt_action st; read_valid ()
        in
        read_valid ()
      in

      let st =
        match make_move st mv with
        | Ok s -> s
        | Error Move_error.Illegal_move ->
            print_endline "Illegal move (game logic). Try again.";
            press_enter "Press Enter..."; st
        | Error Move_error.Game_over -> st
      in

      (match st.stage, st.decision with
       | Finished, _ -> ()
       | _, In_progress { whose_turn = nextp } ->
           let next_name = player_name nextp p1_name p2_name in
           press_enter (Printf.sprintf "Pass to %s and press Enter..." next_name)
       | _ -> ());

      turn_loop st p1_name p2_name
  end

let () =
  clear_screen ();
  print_endline "Two-player Texas Hold'em (no raises)";
  print_string "Enter Player 1 name: "; let p1 = read_line () in
  print_string "Enter Player 2 name: "; let p2 = read_line () in
  match create () with
  | Error _ -> prerr_endline "Failed to create game."
  | Ok st -> turn_loop st p1 p2
