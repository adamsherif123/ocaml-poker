type rank = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
type suit = Clubs|Diamonds|Hearts|Spades
type card = { rank: rank; suit: suit }

type player = P1 | P2

(* Streets of Texas Hold'em *)
type stage = Predeal | Preflop | Flop | Turn | River | Showdown | Finished

type decision =
  | In_progress of { whose_turn: player }
  | Winner of player
  | Stalemate

(* Now includes Check *)
type move = Fold | Check | Call | Bet of int

type t = {
  deck: card list;
  p1: card list;      (* P1 hole cards (2) *)
  p2: card list;      (* P2 hole cards (2) *)
  board: card list;   (* community cards 0..5 *)
  pot: int;
  to_call: int;       (* amount current player must call *)
  stage: stage;
  decision: decision;
  last_check: bool;   (* has the previous player checked with to_call=0? *)
}

module Create_error : sig
  type t = | Invalid_deck
end

val create : unit -> (t, Create_error.t) result

module Move_error : sig
  type t = | Game_over | Illegal_move
end

val make_move : t -> move -> (t, Move_error.t) result

(* Hand evaluation on 5 cards (used internally for best-of-7 at showdown) *)
type hand_rank =
  | High_card of int list
  | Pair of int * int list
  | Two_pair of int * int * int
  | Three_kind of int * int list
  | Straight of int
  | Flush of int list
  | Full_house of int * int
  | Four_kind of int * int
  | Straight_flush of int

val eval_hand : card list -> hand_rank

(* Compare two players given hole+board (7 cards max). >0 if first wins *)
val compare_holes_with_board : card list -> card list -> card list -> int

(* Naive bot that picks legal action *)
val choose_move : t -> move
