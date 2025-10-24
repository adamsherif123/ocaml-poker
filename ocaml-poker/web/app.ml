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

let row cls = let d = Dom_html.createDiv doc in d##.className := Js.string ("row " ^ cls); d
let pill s = let d = Dom_html.createDiv doc in d##.className := Js.string "pill"; Dom.appendChild d (text s); d
let add el child = Dom.appendChild el child
let add_text el s = Dom.appendChild el (text s)

(* ---- App state ---- *)
let state : Poker.t option ref = ref None
let p1_name = ref "Player 1"
let p2_name = ref "Player 2"

(* per-player reveal toggles *)
let p1_reveal = ref false
let p2_reveal = ref false

(* Big blind rotation: true => P1 is BB this hand, false => P2 is BB *)
let bb_is_p1 = ref true
let bb_amount = 5

let rec render () =
  (* If we are at Showdown, resolve to Finished once so we reveal winners correctly *)
  (match !state with
   | Some st when st.Poker.stage = Poker.Showdown ->
       (match Poker.make_move st Poker.Check with
        | Ok st' -> state := Some st'
        | Error _ -> ());
       ()
   | _ -> ());

  let app = by_id "app" in
  while Js.Opt.test (app##.firstChild) do
    Js.Opt.iter app##.firstChild (fun n -> Dom.removeChild app n)
  done;

  let table = Dom_html.createDiv doc in
  table##.className := Js.string "table";

  begin match !state with
  | None ->
      let warn = Dom_html.createDiv doc in
      warn##.className := Js.string "warn";
      add_text warn "Click Start Hand to begin.";
      add table warn

  | Some st ->
      (* header pills *)
      let chips = row "" in
      add chips (pill (match st.Poker.stage with
        | Predeal->"Stage: Predeal" | Preflop->"Stage: Preflop" | Flop->"Stage: Flop"
        | Turn->"Stage: Turn" | River->"Stage: River" | Showdown->"Stage: Showdown"
        | Finished->"Stage: Finished"));
      add chips (pill ("Pot: " ^ string_of_int st.pot));
      add chips (pill ("To call: " ^ string_of_int st.to_call));
      add table chips;

      (* board *)
      let board_row = row "" in
      let label = Dom_html.createSpan doc in label##.className := Js.string "section-title"; add_text label "Board";
      let cards = Dom_html.createDiv doc in cards##.className := Js.string "cards";
      List.iter (fun c -> add cards (card_el c)) st.board;
      add board_row label; add board_row cards; add table board_row;

      (* seats with Show/Hide toggles; auto-reveal at Finished *)
      let seats = Dom_html.createDiv doc in seats##.className := Js.string "seats";
      let seat name cards reveal_ref auto_reveal =
        let wrap = Dom_html.createDiv doc in wrap##.className := Js.string "seat";
        let h = Dom_html.createH3 doc in add_text h name; add wrap h;

        let rowc = Dom_html.createDiv doc in rowc##.className := Js.string "cards";
        let visible = !reveal_ref || auto_reveal in
        (match visible, cards with
         | true, (a::b::_) -> add rowc (card_el a); add rowc (card_el b)
         | true, [a] -> add rowc (card_el a)
         | true, [] -> ()
         | false, _ -> add rowc (card_back ()); add rowc (card_back ()));
        add wrap rowc;

        let btn = Dom_html.createButton doc in
        btn##.className := Js.string "reveal";
        add btn (text (if !reveal_ref then "Hide my cards" else "Show my cards"));
        btn##.onclick := Dom_html.handler (fun _ -> reveal_ref := not !reveal_ref; render (); Js._false);
        add wrap btn;
        wrap
      in
      let auto_show = (st.Poker.stage = Finished) in
      add seats (seat !p1_name st.p1 p1_reveal auto_show);
      add seats (seat !p2_name st.p2 p2_reveal auto_show);
      add table seats;

      (* actions *)
      let actions = Dom_html.createDiv doc in actions##.className := Js.string "actions";
      let add_btn label cb =
        let b = Dom_html.createButton doc in add b (text label);
        b##.onclick := Dom_html.handler (fun _ -> cb (); Js._false);
        add actions b
      in
      let amount_input () =
        let i = Dom_html.createInput ~_type:(Js.string "number") doc in
        i##.value := Js.string "5"; i
      in
      let do_move mv =
        match Poker.make_move st mv with
        | Ok st' -> state := Some st'; render ()
        | Error Poker.Move_error.Illegal_move ->
            (by_id "msg")##.textContent := Js.Opt.return (Js.string "Illegal here")
        | Error Poker.Move_error.Game_over ->
            state := Some st; render ()
      in
      begin match st.Poker.stage with
      | Finished ->
          let done_ = Dom_html.createSpan doc in add_text done_ "Hand over."; add actions done_
      | _ ->
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
  end;

  add app table

let () =
  let start = by_id "start" in
  start##.onclick := Dom_html.handler (fun _ ->
    let get_input id =
      Js.Opt.get (Dom_html.CoerceTo.input (by_id id)) (fun () -> assert false)
    in
    let p1 = Js.to_string (get_input "p1")##.value
    and p2 = Js.to_string (get_input "p2")##.value in
    if String.length p1 > 0 then p1_name := p1;
    if String.length p2 > 0 then p2_name := p2;

    (* reset toggles each new hand *)
    p1_reveal := false; p2_reveal := false;

    (* Create game, then immediately DEAL (so players can reveal right away) *)
    let st0 =
      match Poker.create () with
      | Ok s -> s
      | Error _ ->
          (by_id "msg")##.textContent := Js.Opt.return (Js.string "Failed to init.");
          (* keep old state if init failed *)
          (match !state with Some s -> s | None -> Obj.magic 0)
    in
    let st1 =
      match Poker.make_move st0 Poker.Check with
      | Ok s -> s
      | Error _ -> st0
    in
    (* Apply rotating big blind of 5 *)
    let st2 =
      if !bb_is_p1 then (
        (* P1 is big blind: P1 bets 5; P2 acts facing 5 *)
        match Poker.make_move st1 (Poker.Bet bb_amount) with
        | Ok s -> s
        | Error _ -> st1
      ) else (
        (* P2 is big blind: pass turn to P2 via P1 check, then P2 bets 5 *)
        let s1 = match Poker.make_move st1 Poker.Check with Ok s -> s | Error _ -> st1 in
        match Poker.make_move s1 (Poker.Bet bb_amount) with
        | Ok s -> s
        | Error _ -> s1
      )
    in
    (* Toggle for next hand *)
    bb_is_p1 := not !bb_is_p1;

    state := Some st2; render (); Js._false
  );
  render ()
