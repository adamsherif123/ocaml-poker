let () = Random.self_init ()

type rank = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
type suit = Clubs|Diamonds|Hearts|Spades
type card = { rank: rank; suit: suit }

type player = P1 | P2

type stage = Predeal | Preflop | Flop | Turn | River | Showdown | Finished

type decision =
  | In_progress of { whose_turn: player }
  | Winner of player
  | Stalemate

type move = Fold | Check | Call | Bet of int

type t = {
  deck: card list;
  p1: card list;
  p2: card list;
  board: card list;
  pot: int;
  to_call: int;
  stage: stage;
  decision: decision;
  last_check: bool;
}

module Create_error = struct
  type t = | Invalid_deck
end

module Move_error = struct
  type t = | Game_over | Illegal_move
end

let all_ranks = [Two;Three;Four;Five;Six;Seven;Eight;Nine;Ten;Jack;Queen;King;Ace]
let all_suits = [Clubs;Diamonds;Hearts;Spades]

let rank_to_int = function
  | Two->2 | Three->3 | Four->4 | Five->5 | Six->6 | Seven->7 | Eight->8
  | Nine->9 | Ten->10 | Jack->11 | Queen->12 | King->13 | Ace->14

let build_deck () =
  List.concat (List.map (fun s ->
    List.map (fun r -> {rank=r; suit=s}) all_ranks) all_suits)

let shuffle lst =
  let a = Array.of_list lst in
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i+1) in
    let tmp = a.(i) in
    a.(i) <- a.(j); a.(j) <- tmp
  done;
  Array.to_list a

let create () =
  let d = shuffle (build_deck ()) in
  Ok {
    deck = d; p1 = []; p2 = []; board = [];
    pot = 0; to_call = 0;
    stage = Predeal; decision = In_progress { whose_turn = P1 };
    last_check = false;
  }

(* -------- 5-card hand ranking -------- *)
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

let counts_by_rank cards =
  let tbl = Hashtbl.create 16 in
  List.iter (fun c ->
    let k = rank_to_int c.rank in
    Hashtbl.replace tbl k (1 + (match Hashtbl.find_opt tbl k with Some n->n | None->0))
  ) cards;
  let pairs = Hashtbl.fold (fun k v acc -> (k,v)::acc) tbl [] in
  List.sort (fun (k1,v1) (k2,v2) ->
    let c = compare v2 v1 in if c<>0 then c else compare k2 k1) pairs

let is_flush cards =
  match cards with
  | [] -> false
  | c::cs -> List.for_all (fun x -> x.suit = c.suit) cs

let is_straight cards =
  let rs = List.map (fun c -> rank_to_int c.rank) cards |> List.sort compare in
  (* wheel: A2345 *)
  let rs = if rs = [2;3;4;5;14] then [1;2;3;4;5] else rs in
  let rec aux = function
    | a::b::rest when b = a+1 -> aux (b::rest)
    | [_] -> true
    | _ -> false
  in
  (List.length rs = 5) && aux rs, List.hd (List.rev rs)

let eval_hand cards =
  let flush = is_flush cards in
  let straight, hi = is_straight cards in
  if flush && straight then Straight_flush hi
  else
    match counts_by_rank cards with
    | (k,4)::(k2,1)::_ -> Four_kind (k, k2)
    | (k3,3)::(k2,2)::_ -> Full_house (k3,k2)
    | _ when flush ->
        let ks = cards |> List.map (fun c -> rank_to_int c.rank)
                       |> List.sort compare |> List.rev in
        Flush ks
    | _ when straight -> Straight hi
    | (k3,3)::rest ->
        let kickers = rest |> List.map fst |> List.sort compare |> List.rev in
        Three_kind (k3, kickers)
    | (k1,2)::(k2,2)::rest ->
        let hi = max k1 k2 and lo = min k1 k2 in
        let kicker = match rest with (k,_)::_ -> k | _ -> 0 in
        Two_pair (hi, lo, kicker)
    | (k1,2)::rest ->
        let kickers = rest |> List.map fst |> List.sort compare |> List.rev in
        Pair (k1, kickers)
    | _ ->
        let ks = cards |> List.map (fun c -> rank_to_int c.rank)
                       |> List.sort compare |> List.rev in
        High_card ks

let compare_lists xs ys =
  let rec go a b =
    match a,b with
    | [],[] -> 0
    | x::ax, y::by -> let c = compare x y in if c<>0 then c else go ax by
    | _ -> 0
  in go xs ys

let compare_5 h1 h2 =
  let r1 = eval_hand h1 and r2 = eval_hand h2 in
  let ord = match r1, r2 with
  | Straight_flush a, Straight_flush b -> compare a b
  | Four_kind (a,ka), Four_kind (b,kb) ->
      let c = compare a b in if c<>0 then c else compare ka kb
  | Full_house (a1,b1), Full_house (a2,b2) ->
      let c = compare a1 a2 in if c<>0 then c else compare b1 b2
  | Flush xs, Flush ys -> compare_lists xs ys
  | Straight a, Straight b -> compare a b
  | Three_kind (a,kx), Three_kind (b,ky) ->
      let c = compare a b in if c<>0 then c else compare_lists kx ky
  | Two_pair (a1,b1,k1), Two_pair (a2,b2,k2) ->
      let c = compare a1 a2 in if c<>0 then c else
      let d = compare b1 b2 in if d<>0 then d else compare k1 k2
  | Pair (p1,kx), Pair (p2,ky) ->
      let c = compare p1 p2 in if c<>0 then c else compare_lists kx ky
  | High_card xs, High_card ys -> compare_lists xs ys
  | Straight_flush _, _ -> 1
  | _, Straight_flush _ -> -1
  | Four_kind _, _ -> 1
  | _, Four_kind _ -> -1
  | Full_house _, _ -> 1
  | _, Full_house _ -> -1
  | Flush _, _ -> 1
  | _, Flush _ -> -1
  | Straight _, _ -> 1
  | _, Straight _ -> -1
  | Three_kind _, _ -> 1
  | _, Three_kind _ -> -1
  | Two_pair _, _ -> 1
  | _, Two_pair _ -> -1
  | Pair _, _ -> 1
  | _, Pair _ -> -1
  in
  ord

(* choose best 5 out of up to 7 cards *)
let combos_5 lst =
  let rec choose k xs =
    match k, xs with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | k, x::xt ->
        let with_x = List.map (fun rest -> x::rest) (choose (k-1) xt) in
        let without_x = choose k xt in
        with_x @ without_x
  in
  choose 5 lst

let compare_holes_with_board h1 h2 board =
  let pool1 = h1 @ board in
  let pool2 = h2 @ board in
  let best1 =
    combos_5 pool1 |> List.fold_left (fun acc cand ->
      match acc with None -> Some cand
      | Some cur -> if compare_5 cand cur > 0 then Some cand else acc
    ) None |> Option.get
  in
  let best2 =
    combos_5 pool2 |> List.fold_left (fun acc cand ->
      match acc with None -> Some cand
      | Some cur -> if compare_5 cand cur > 0 then Some cand else acc
    ) None |> Option.get
  in
  compare_5 best1 best2

let draw_n n deck =
  let rec take k d acc =
    if k=0 then (List.rev acc, d)
    else match d with
      | x::xs -> take (k-1) xs (x::acc)
      | [] -> (List.rev acc, [])
  in take n deck []

let to_other = function P1 -> P2 | P2 -> P1

let deal_holes st =
  let (p1h, d1) = draw_n 2 st.deck in
  let (p2h, d2) = draw_n 2 d1 in
  { st with deck = d2; p1 = p1h; p2 = p2h }

let reveal_flop st =
  let (flop, d1) = draw_n 3 st.deck in
  { st with deck = d1; board = flop }

let reveal_turn st =
  let (t, d1) = draw_n 1 st.deck in
  { st with deck = d1; board = st.board @ t }

let reveal_river st =
  let (r, d1) = draw_n 1 st.deck in
  { st with deck = d1; board = st.board @ r }

let advance_street st =
  match st.stage with
  | Preflop ->
      let st = reveal_flop st in
      { st with stage = Flop; to_call = 0; last_check = false; decision = In_progress { whose_turn = P1 } }
  | Flop ->
      let st = reveal_turn st in
      { st with stage = Turn; to_call = 0; last_check = false; decision = In_progress { whose_turn = P1 } }
  | Turn ->
      let st = reveal_river st in
      { st with stage = River; to_call = 0; last_check = false; decision = In_progress { whose_turn = P1 } }
  | River ->
      { st with stage = Showdown }
  | _ -> st

let showdown st =
  let c = compare_holes_with_board st.p1 st.p2 st.board in
  let winner = if c > 0 then P1 else if c < 0 then P2 else P1 in
  { st with stage = Finished; decision = Winner winner }

let make_move st mv =
  match st.decision with
  | Winner _ | Stalemate -> Error Move_error.Game_over
  | In_progress { whose_turn } ->
    begin match st.stage, mv with
    | Predeal, _ ->
        let st = deal_holes st in
        Ok { st with stage = Preflop; decision = In_progress { whose_turn = P1 };
                     to_call = 0; last_check = false; pot = 0 }
    | (Preflop | Flop | Turn | River), Fold ->
        let winner = to_other whose_turn in
        Ok { st with stage = Finished; decision = Winner winner }
    | (Preflop | Flop | Turn | River), Check ->
        if st.to_call <> 0 then Error Move_error.Illegal_move
        else
          if st.last_check then
            let st' = advance_street st in
            if st'.stage = Showdown then Ok (showdown st')
            else Ok { st' with decision = In_progress { whose_turn = P1 };
                              last_check = false }
          else
            Ok { st with decision = In_progress { whose_turn = to_other whose_turn };
                        last_check = true }
    | (Preflop | Flop | Turn | River), Bet n ->
        if n <= 0 || st.to_call <> 0 then Error Move_error.Illegal_move
        else
          Ok { st with pot = st.pot + n; to_call = n; last_check = false;
                      decision = In_progress { whose_turn = to_other whose_turn } }
    | (Preflop | Flop | Turn | River), Call ->
        if st.to_call = 0 then Error Move_error.Illegal_move
        else
          let st' = { st with pot = st.pot + st.to_call; to_call = 0 } in
          let st'' = advance_street st' in
          if st''.stage = Showdown then Ok (showdown st'')
          else Ok { st'' with decision = In_progress { whose_turn = P1 };
                             last_check = false }
    | Showdown, _ ->
        Ok (showdown st)
    | Finished, _ ->
        Error Move_error.Game_over
    end

(* --- Naive bot --- *)
let has_pair cards =
  let tbl = Hashtbl.create 8 in
  List.iter (fun c ->
    let k = rank_to_int c.rank in
    Hashtbl.replace tbl k (1 + (match Hashtbl.find_opt tbl k with Some n->n | None->0))
  ) cards;
  Hashtbl.to_seq tbl |> Seq.exists (fun (_,v)-> v>=2)

let choose_move st =
  match st.stage with
  | Preflop | Flop | Turn | River ->
      if st.to_call = 0 then
        let my =
          match st.decision with
          | In_progress { whose_turn = P1 } -> st.p1
          | _ -> st.p2
        in
        if has_pair (my @ st.board) then Bet 5 else Check
      else
        let my =
          match st.decision with
          | In_progress { whose_turn = P1 } -> st.p1
          | _ -> st.p2
        in
        if has_pair (my @ st.board) then Call else Fold
  | _ -> Check
