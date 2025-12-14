open Js_of_ocaml

let doc = Dom_html.document
let by_id id =
  Js.Opt.get (doc##getElementById (Js.string id)) (fun () -> assert false)

let text s = doc##createTextNode (Js.string s)

(* ---- Card helpers ---- *)
let rank_str = function
  | Poker.Two->"2"|Three->"3"|Four->"4"|Five->"5"|Six->"6"|Seven->"7"|Eight->"8"
  | Nine->"9"|Ten->"T"|Jack->"J"|Queen->"Q"|King->"K"|Ace->"A"

let suit_char = function
  | Poker.Clubs->"♣" | Diamonds->"♦" | Hearts->"♥" | Spades->"♠"

let is_red = function Poker.Hearts | Poker.Diamonds -> true | _ -> false

let card_el (c:Poker.card) =
  let root = Dom_html.createDiv doc in
  root##.className := Js.string ("playing-card " ^ (if is_red c.suit then "red" else "black"));
  let r = Dom_html.createDiv doc in r##.className := Js.string "rank"; Dom.appendChild r (text (rank_str c.rank));
  let s = Dom_html.createDiv doc in s##.className := Js.string "suit"; Dom.appendChild s (text (suit_char c.suit));
  let pip = Dom_html.createDiv doc in pip##.className := Js.string "pip"; Dom.appendChild pip (text (suit_char c.suit));
  Dom.appendChild root r; Dom.appendChild root s; Dom.appendChild root pip; root

let card_back () =
  let b = Dom_html.createDiv doc in
  b##.className := Js.string "card-back"; b

let row cls =
  let d = Dom_html.createDiv doc in
  d##.className := Js.string ("row " ^ cls);
  d

let pill s =
  let d = Dom_html.createDiv doc in
  d##.className := Js.string "pill";
  Dom.appendChild d (text s);
  d

let add el child = Dom.appendChild el child
let add_text el s = Dom.appendChild el (text s)

(* ---- Firebase-backed user (P1) ---- *)
let user_uid  : string option ref = ref None
let user_email = ref ""
let user_name  = ref ""   (* stored in Firestore *)

let p1_stack = ref 0       (* persisted to Firestore *)
let p2_stack = ref 1000    (* local guest for now *)
let hand_settled = ref false

let try_set_msg s =
  try (by_id "msg")##.textContent := Js.Opt.return (Js.string s) with _ -> ()

let fb_save_p1 () =
  match !user_uid with
  | None -> ()
  | Some _ ->
      try
        let fn = Js.Unsafe.get (Js.Unsafe.global) "fbSaveChips" in
        ignore (Js.Unsafe.fun_call fn [| Js.Unsafe.inject (float_of_int !p1_stack) |])
      with _ -> ()

let fb_save_name (nm:string) =
  match !user_uid with
  | None -> ()
  | Some _ ->
      try
        let fn = Js.Unsafe.get (Js.Unsafe.global) "fbSaveName" in
        ignore (Js.Unsafe.fun_call fn [| Js.Unsafe.inject (Js.string nm) |])
      with _ -> ()

let js_str (x:'a) : string =
  Js.to_string (Js.Unsafe.coerce x : Js.js_string Js.t)

let js_int (x:'a) : int =
  (* FIX for your build error: JS numbers are floats *)
  int_of_float (Js.float_of_number (Js.Unsafe.coerce x : Js.number Js.t))

let set_input_value id v =
  try
    let inp = Js.Opt.get (Dom_html.CoerceTo.input (by_id id)) (fun () -> assert false) in
    inp##.value := Js.string v
  with _ -> ()

(* ---- Game state ---- *)
let state : Poker.t option ref = ref None
let p1_name = ref "Player 1"
let p2_name = ref "Player 2"
let p1_reveal = ref false
let p2_reveal = ref false

let bb_is_p1 = ref true
let bb_amount = 5

let whose_turn (st:Poker.t) =
  match st.Poker.decision with
  | Poker.In_progress { whose_turn } -> whose_turn
  | _ -> Poker.P1

let cost_for_move (st:Poker.t) = function
  | Poker.Bet n -> n
  | Poker.Call -> st.to_call
  | _ -> 0

let apply_cost (pl:Poker.player) (cost:int) =
  if cost <= 0 then true
  else match pl with
    | Poker.P1 ->
        if !p1_stack < cost then (try_set_msg "Not enough chips (P1)."; false)
        else (p1_stack := !p1_stack - cost; fb_save_p1 (); true)
    | Poker.P2 ->
        if !p2_stack < cost then (try_set_msg "Not enough chips (P2)."; false)
        else (p2_stack := !p2_stack - cost; true)

let maybe_settle (st:Poker.t) =
  if !hand_settled then ()
  else if st.Poker.stage <> Poker.Finished then ()
  else begin
    match st.Poker.decision with
    | Poker.Winner Poker.P1 ->
        p1_stack := !p1_stack + st.pot;
        fb_save_p1 ();
        hand_settled := true
    | Poker.Winner Poker.P2 ->
        p2_stack := !p2_stack + st.pot;
        hand_settled := true
    | _ -> hand_settled := true
  end

let rec render () =
  (* resolve Showdown -> Finished once *)
  (match !state with
   | Some st when st.Poker.stage = Poker.Showdown ->
       (match Poker.make_move st Poker.Check with
        | Ok st' -> state := Some st'
        | Error _ -> ());
       ()
   | _ -> ());

  (match !state with Some st -> maybe_settle st | None -> ());

  let app = by_id "app" in
  while Js.Opt.test (app##.firstChild) do
    Js.Opt.iter app##.firstChild (fun n -> Dom.removeChild app n)
  done;

  let table = Dom_html.createDiv doc in
  table##.className := Js.string "table";

  begin match !user_uid with
  | None ->
      let warn = Dom_html.createDiv doc in
      warn##.className := Js.string "warn";
      add_text warn "Please sign in above to load your chips.";
      add table warn
  | Some _ ->
      begin match !state with
      | None ->
          let warn = Dom_html.createDiv doc in
          warn##.className := Js.string "warn";
          add_text warn ("Welcome, " ^ !user_name ^ ". Click Start hand to begin.");
          add table warn
      | Some st ->
          (* header pills (NO email, NO chip counts here) *)
          let chips = row "" in
          add chips (pill (match st.Poker.stage with
            | Predeal->"Stage: Predeal" | Preflop->"Stage: Preflop" | Flop->"Stage: Flop"
            | Turn->"Stage: Turn" | River->"Stage: River" | Showdown->"Stage: Showdown"
            | Finished->"Stage: Finished"));
          add chips (pill ("Pot: " ^ string_of_int st.pot));
          add chips (pill ("To call: " ^ string_of_int st.to_call));
          add table chips;

          (* board *)
          let board_row = row "board-row" in
          let label = Dom_html.createSpan doc in
          label##.className := Js.string "section-title";
          add_text label "Board";
          let cards = Dom_html.createDiv doc in
          cards##.className := Js.string "cards";
          List.iter (fun c -> add cards (card_el c)) st.board;
          add board_row label; add board_row cards; add table board_row;

          (* seats w/ chips next to names *)
          let seats = Dom_html.createDiv doc in seats##.className := Js.string "seats";

          let seat (name:string) (stack:int) (cards:Poker.card list) (reveal_ref:bool ref) (auto_reveal:bool) =
            let wrap = Dom_html.createDiv doc in
            wrap##.className := Js.string "seat";

            let head = Dom_html.createDiv doc in
            head##.className := Js.string "seat-head";
            let nm = Dom_html.createSpan doc in nm##.className := Js.string "seat-name";
            let st = Dom_html.createSpan doc in st##.className := Js.string "seat-chips";
            add_text nm name;
            add_text st (string_of_int stack ^ " chips");
            add head nm; add head st; add wrap head;

            let rowc = Dom_html.createDiv doc in rowc##.className := Js.string "cards";
            let visible = !reveal_ref || auto_reveal in
            (match visible, cards with
             | true, (a::b::_) -> add rowc (card_el a); add rowc (card_el b)
             | true, [a] -> add rowc (card_el a)
             | true, [] -> ()
             | false, _ -> add rowc (card_back ()); add rowc (card_back ()));
            add wrap rowc;

            let btn = Dom_html.createButton doc in
            btn##.className := Js.string "btn ghost reveal-btn";
            add btn (text (if !reveal_ref then "Hide my cards" else "Show my cards"));
            btn##.onclick := Dom_html.handler (fun _ -> reveal_ref := not !reveal_ref; render (); Js._false);
            add wrap btn;

            wrap
          in

          let auto_show = (st.Poker.stage = Finished) in
          add seats (seat !p1_name !p1_stack st.p1 p1_reveal auto_show);
          add seats (seat !p2_name !p2_stack st.p2 p2_reveal auto_show);
          add table seats;

          (* actions *)
          let actions = Dom_html.createDiv doc in actions##.className := Js.string "actions";

          let add_btn label cb =
            let b = Dom_html.createButton doc in
            b##.className := Js.string "btn primary";
            add b (text label);
            b##.onclick := Dom_html.handler (fun _ -> cb (); Js._false);
            add actions b
          in

          let amount_input () =
            let i = Dom_html.createInput ~_type:(Js.string "number") doc in
            i##.value := Js.string "5";
            i
          in

          let do_move mv =
            let pl = whose_turn st in
            let cost = cost_for_move st mv in
            if apply_cost pl cost then
              match Poker.make_move st mv with
              | Ok st' -> state := Some st'; render ()
              | Error Poker.Move_error.Illegal_move -> try_set_msg "Illegal move here."
              | Error Poker.Move_error.Game_over -> try_set_msg "Game over."
          in

          begin match st.Poker.stage with
          | Finished ->
              let done_ = Dom_html.createSpan doc in add_text done_ "Hand over."; add actions done_
          | _ ->
              let pl = whose_turn st in
              let turn_txt = match pl with Poker.P1 -> (!p1_name ^ "'s turn") | Poker.P2 -> (!p2_name ^ "'s turn") in
              add actions (pill turn_txt);

              if st.to_call = 0 then begin
                add_btn "Check" (fun () -> do_move Poker.Check);
                let amt = amount_input () in add actions amt;
                add_btn "Bet" (fun () ->
                  let n = match int_of_string_opt (Js.to_string amt##.value) with Some x when x>0 -> x | _ -> 0 in
                  do_move (Poker.Bet n));
                add_btn "Fold" (fun () -> do_move Poker.Fold)
              end else begin
                add_btn ("Call " ^ string_of_int st.to_call) (fun () -> do_move Poker.Call);
                add_btn "Fold" (fun () -> do_move Poker.Fold)
              end
          end;

          add table actions
      end
  end;

  add app table

let () =
  (* Called by firebase.js after login: ocamlSetUser(uid, email, chips, name) *)
  Js.Unsafe.set (Js.Unsafe.global) "ocamlSetUser"
    (Js.wrap_callback (fun uid email chips name ->
       user_uid := Some (js_str uid);
       user_email := js_str email;
       user_name := js_str name;
       p1_stack := js_int chips;

       (* Default P1 name is the stored name, and keep it in the textbox (editable) *)
       p1_name := !user_name;
       set_input_value "p1" !user_name;

       try_set_msg "";
       state := None;
       render ()));

  Js.Unsafe.set (Js.Unsafe.global) "ocamlClearUser"
    (Js.wrap_callback (fun () ->
       user_uid := None;
       user_email := "";
       user_name := "";
       p1_stack := 0;
       state := None;
       try_set_msg "";
       render ()));

  let start = by_id "start" in
  start##.onclick := Dom_html.handler (fun _ ->
    match !user_uid with
    | None -> try_set_msg "Sign in first."; Js._false
    | Some _ ->
      let get_input id =
        Js.Opt.get (Dom_html.CoerceTo.input (by_id id)) (fun () -> assert false)
      in
      let p1_in = Js.to_string (get_input "p1")##.value
      and p2_in = Js.to_string (get_input "p2")##.value in

      if String.length p1_in > 0 then (
        p1_name := p1_in;
        (* If user changed name, persist it *)
        if p1_in <> !user_name then (user_name := p1_in; fb_save_name p1_in)
      ) else (
        (* keep it always filled *)
        set_input_value "p1" !user_name;
        p1_name := !user_name
      );

      if String.length p2_in > 0 then p2_name := p2_in;

      p1_reveal := false; p2_reveal := false;
      hand_settled := false;

      (* create + deal holes immediately *)
      let st0 =
        match Poker.create () with
        | Ok s -> s
        | Error _ -> try_set_msg "Failed to init."; (match !state with Some s -> s | None -> Obj.magic 0)
      in
      let st1 = match Poker.make_move st0 Poker.Check with Ok s -> s | Error _ -> st0 in

      (* apply big blind rotation *)
      let st2 =
        if !bb_is_p1 then (
          if !p1_stack < bb_amount then (try_set_msg "P1 doesn't have enough chips for BB."; st1)
          else (
            p1_stack := !p1_stack - bb_amount; fb_save_p1 ();
            match Poker.make_move st1 (Poker.Bet bb_amount) with Ok s -> s | Error _ -> st1
          )
        ) else (
          if !p2_stack < bb_amount then (try_set_msg "P2 doesn't have enough chips for BB."; st1)
          else (
            p2_stack := !p2_stack - bb_amount;
            let s1 = match Poker.make_move st1 Poker.Check with Ok s -> s | Error _ -> st1 in
            match Poker.make_move s1 (Poker.Bet bb_amount) with Ok s -> s | Error _ -> s1
          )
        )
      in
      bb_is_p1 := not !bb_is_p1;

      state := Some st2;
      render ();
      Js._false
  );

  render ()