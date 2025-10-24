open Poker

let show_card c =
  let r = match c.rank with
    | Two->"2"|Three->"3"|Four->"4"|Five->"5"|Six->"6"|Seven->"7"|Eight->"8"
    | Nine->"9"|Ten->"T"|Jack->"J"|Queen->"Q"|King->"K"|Ace->"A" in
  let s = match c.suit with Clubs->"♣"|Diamonds->"♦"|Hearts->"♥"|Spades->"♠" in
  r ^ s

let show_hand h = String.concat " " (List.map show_card h)
let show_board b = if b = [] then "(no board yet)" else show_hand b

let prompt_action st =
  if st.to_call = 0 then
    print_string "Action (check/bet N/fold): "
  else
    Printf.printf "Facing bet of %d. Action (call/fold): " st.to_call

let parse_action line to_call =
  let line = String.lowercase_ascii (String.trim line) in
  match line with
  | "fold" -> Fold
  | "check" -> Check
  | "call" -> Call
  | _ when String.length line >= 3 && String.sub line 0 3 = "bet" ->
      if to_call <> 0 then Call
      else
        let amt =
          try int_of_string (String.trim (String.sub line 3 (String.length line - 3)))
          with _ -> 0
        in Bet amt
  | _ -> if to_call = 0 then Check else Call

let rec loop st =
  (* Ensure we start by dealing if needed *)
  let st =
    match st.stage with
    | Predeal -> Result.get_ok (make_move st Check)
    | _ -> st
  in
  Printf.printf "\nYour hand: %s\nBoard: %s\nPot: %d\n\n"
    (show_hand st.p1) (show_board st.board) st.pot;
  match st.stage with
  | Showdown ->
      let st = Result.get_ok (make_move st Check) in
      loop st
  | Finished ->
      begin match st.decision with
      | Winner P1 -> Printf.printf "You win! Pot=%d\n" st.pot
      | Winner P2 -> Printf.printf "Bot wins! Pot=%d\n" st.pot
      | _ -> Printf.printf "Stalemate.\n"
      end;
      st |> ignore
  | _ ->
      prompt_action st;
      let mv = parse_action (read_line ()) st.to_call in
      let st = match make_move st mv with Ok s -> s | Error _ -> st in
      let st =
        match st.decision with
        | In_progress { whose_turn = P2 } ->
            let mvb = choose_move st in
            Printf.printf "Bot plays: %s\n"
              (match mvb with
               | Fold -> "Fold"
               | Check -> "Check"
               | Call -> "Call"
               | Bet n -> "Bet " ^ string_of_int n);
            (match make_move st mvb with Ok s -> s | Error _ -> st)
        | _ -> st
      in
      loop st

let () =
  match create () with
  | Error _ -> prerr_endline "Failed to create game."
  | Ok st -> ignore (loop st)
