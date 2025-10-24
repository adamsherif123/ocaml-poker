open OUnit2
open Poker

let mk_test_eval _ =
  let hand_high =
    [ {rank=Ace; suit=Spades}; {rank=Seven; suit=Clubs};
      {rank=Four; suit=Hearts}; {rank=Three; suit=Diamonds};
      {rank=Ten; suit=Clubs} ] in
  match eval_hand hand_high with
  | High_card _ -> assert_bool "ok" true
  | _ -> assert_failure "expected High_card"

let mk_test_compare_holdem _ =
  (* Common board (3 cards to make 5 with holes) *)
  let board =
    [ {rank=Four; suit=Hearts};
      {rank=Three; suit=Diamonds};
      {rank=Ten;  suit=Clubs} ] in
  let p1_holes = [ {rank=King; suit=Spades}; {rank=King; suit=Clubs} ] in
  let p2_holes = [ {rank=Ace;  suit=Spades}; {rank=Seven; suit=Clubs} ] in
  assert_bool "pair (K) beats high-card"
    (compare_holes_with_board p1_holes p2_holes board > 0)

let suite =
  "poker" >::: [
    "eval_high" >:: mk_test_eval;
    "compare_holdem" >:: mk_test_compare_holdem;
  ]

let () = run_test_tt_main suite
