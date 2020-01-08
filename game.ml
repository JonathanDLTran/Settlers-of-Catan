open Player
open Board
open Command
open Pregame

type game_info = 
  Player.t * Player.t * Board.t

type player = bool

type game = 
  | GameContinue of game_info
  | GameVictory of player
  | GameQuit

let return_game (p1, p2, board) = 
  GameContinue (p1, p2, board)

let bind f game = 
  match game with 
  | GameQuit -> GameQuit
  | GameVictory p -> GameVictory p
  | GameContinue p -> f p

let (>>=) = bind 

let pregame_to_game pregame = 
  match pregame with 
  | PreQuit -> GameQuit
  | PreContinue (p1, p2, board) -> GameContinue (p1, p2, board)

let end_game game = 
  match game with 
  | GameContinue (_, _, _) -> failwith "Impossible case: All games must terminate. "
  | GameQuit -> ANSITerminal.(print_string [green] "Exiting...")
  | GameVictory p -> 
    if p then ANSITerminal.(print_string [green] "Player 1 wins")
    else ANSITerminal.(print_string [green] "Player 2 wins")

(* ######## MESSAGES ######### *)

let player_turn_msg player = 
  if player then 
    ANSITerminal.(print_string [blue] "\nPlayer 1 to move. \n")
  else 
    ANSITerminal.(print_string [blue] "\nPlayer 2 to move. \n")

let invalid_msg () = 
  ANSITerminal.(print_string [blue] "\nInvalid Command. \n")

let quit_msg () = 
  ANSITerminal.(print_string [green] "Quitting game. ")

let buy_dev_card_msg () = 
  ANSITerminal.(print_string [green] "Purchasing a development card... ")

let cheat_msg () = 
  ANSITerminal.(print_string [green] "Cheat activated... +100 resources each ")

let lack_resources_msg () = 
  ANSITerminal.(print_string [green] "You do not have enough resources to build the structure. ")

let not_enough_pieces_msg () = 
  ANSITerminal.(print_string [green] "You do not have any more of the structure that you want to build. ")

let not_enough_dev_cards_msg () = 
  ANSITerminal.(print_string [green] "There are no more development cards to buy. ")

let move_robber_msg tile = 
  ANSITerminal.(print_string [green] ("Move the robber to another tile other than " ^ tile ^ ". "));
  ANSITerminal.(print_string [green] "Please type in a character between A and S indicating a tile. ")

let robber_new_tile_msg tile = 
  ANSITerminal.(print_string [green] ("The robber was moved to tile " ^ tile ^ ". "))

let can_steal_msg () = 
  ANSITerminal.(print_string [green] "You may steal one card from your opponent as you put the robber on a tile your opponent has a structure on.")

let cannot_steal_msg () = 
  ANSITerminal.(print_string [green] "You may not steal one card from your opponent as you did not put the robber on a tile your opponent has a structure on.") 

let opponent_no_cards_msg () = 
  ANSITerminal.(print_string [green] " Your opponent had no cards for you to steal")

let stolen_card_msg resource = 
  (match resource with 
   | Player.Ore -> "ore"
   | Player.Wheat -> "wheat"
   | Player.Wool -> "wool"
   | Player.Lumber -> "lumber"
   | Player.Brick -> "brick")
  |> (fun res -> ANSITerminal.(print_string [green] ("You stole a " ^ res) ))

let marine_trade_msg res1 res2 = 
  ANSITerminal.(print_string [green] ("Attempting marine trade four " ^ res1 ^ " for one " ^ res2 ^ ".\n"))

let marine_trade_success res1 res2 = 
  ANSITerminal.(print_string [green] ("Successfully traded four " ^ res1 ^ " for one " ^ res2 ^ ".\n"))

let marine_trade_fail () = 
  ANSITerminal.(print_string [green] "Marine trade failed. ")

(* ########## GAME UTILITIES ####### *)

let roll () = 
  Random.self_init ();
  let die1 = (Random.int 6) + 1 in 
  Random.self_init ();
  let die2 = (Random.int 6) + 1 in 
  let dice_roll = die1 + die2 in 
  "You rolled a : " ^ string_of_int dice_roll |> print_endline;
  dice_roll

(* ######## CONVERSIONS ######### *)

let cmd_res_to_plyr_res cmd_res = 
  if cmd_res = "lumber" then Player.Lumber
  else if cmd_res = "ore" then Player.Ore
  else if cmd_res = "wool" then Player.Wool
  else if cmd_res = "brick" then Player.Brick
  else if cmd_res = "wheat" then Player.Wheat
  else failwith "command cannot give another substance"

(* ######## GAME ACTIONS ####### *)

let resource_hoard_to_list hoard = 
  [(Player.Wheat, hoard.wheat);
   (Player.Ore, hoard.ore);
   (Player.Wool, hoard.wool);
   (Player.Lumber, hoard.lumber);
   (Player.Brick, hoard.brick);]

let rec handle_halve_hand player (p1, p2, board) = 
  (halve_hand p1, halve_hand p2, board)

let handle_card_robbery player (p1, p2, board) = 
  if player then begin
    match robber_choose_random_card p1 with
    | Some resource -> 
      stolen_card_msg resource;
      (bulk_add_resources p1 [(resource, 1)], bulk_remove_resources p2 [(resource, 1)], board)
    | None -> 
      opponent_no_cards_msg (); (p1, p2, board)
  end
  else begin
    match robber_choose_random_card p1 with
    | Some resource -> 
      stolen_card_msg resource;
      (bulk_remove_resources p1 [(resource, 1)], bulk_add_resources p2 [(resource, 1)], board)
    | None -> 
      opponent_no_cards_msg (); (p1, p2, board)
  end

let steal_resource player (p1, p2, board) = 
  if can_robber_steal_card (not player) board then begin
    let () = can_steal_msg () in 
    handle_card_robbery player (p1, p2, board)
  end
  else 
    let () = cannot_steal_msg () in
    (p1, p2, board)

let rec require_move_robber robber_tile player (p1, p2, board) = 
  let () = move_robber_msg robber_tile in 
  match () |> read_line |> parse_robber with
  | NotLocation -> require_move_robber robber_tile player (p1, p2, board)
  | NewLocation s -> 
    if s = String.lowercase_ascii robber_tile then require_move_robber robber_tile player (p1, p2, board)
    else 
      let () = robber_new_tile_msg (s |> String.uppercase_ascii) in 
      (p1, p2, set_robber_tile (String.get (String.uppercase_ascii s) 0) board)

let handle_rolling_seven_robber roll player (p1, p2, board) = 
  if roll = 7 then begin
    (if player then (set_robber p1 true, set_robber p2 false, board)
     else (set_robber p1 false, set_robber p2 true, board))
    |> handle_halve_hand player
    |> require_move_robber (get_robber_tile board) player 
    |> steal_resource player (* must come after require_move_helper as the reqiuire_move_helper sets the position of the new robber tile *)
  end 
  else 
    (p1, p2, board)

let handle_roll player (p1, p2, board) = 
  let roll = roll () in 
  let res1, res2 = get_resources roll board in 
  let p1' = res1 |> resource_hoard_to_list |> bulk_add_resources p1 in 
  let p2' = res2 |> resource_hoard_to_list |> bulk_add_resources p2 in 
  (p1', p2', board)
  |> handle_rolling_seven_robber roll player

let deal_with_action_error color error = 
  match error with 
  | PostionOccupiedErr -> ANSITerminal.(print_string [color] "Position was occupied . ")
  | AdjacentPositionErr ->  ANSITerminal.(print_string [color] "You cannot place a structure like a settlement or a city adjacent to another structure. ")
  | UnconnectedErr -> ANSITerminal.(print_string [color] "You must place a road, settlement or city connected to a road of yours. ")
  | SettlmentMissingErr -> ANSITerminal.(print_string [color] "The city at the location you wish to place must have a settlement to upgrade already. ")
  | NotAnEdgeErr ->  ANSITerminal.(print_string [color] "The nodes you specified are not an edge on the board. ")

let handle_show player (p1, p2, board) = 
  if player then print_string_of_player_info p1
  else print_string_of_player_info p2

let color_of_player player = 
  if player then ANSITerminal.blue
  else ANSITerminal.red

let play_settlement player (p1, p2) = 
  if player then (add_piece p1 (Player.Settlement)), p2
  else (p1, add_piece p2 (Player.Settlement))

let play_city player (p1, p2) = 
  if player then (add_piece p1 (Player.City)), p2
  else (p1, add_piece p2 (Player.City))

let play_road player (p1, p2) = 
  if player then (add_piece p1 (Player.Road)), p2
  else (p1, add_piece p2 (Player.Road))

let check_build_road player (p1, p2, board) = 
  if player then begin
    if not (can_build_road p1) then let () = not_enough_pieces_msg () in false 
    else if not (has_resources_road p1) then let () = lack_resources_msg () in false
    else true
  end 
  else begin
    if not (can_build_road p2) then let () = not_enough_pieces_msg () in false 
    else if not (has_resources_road p2) then let () = lack_resources_msg () in false
    else true
  end

let check_build_settlement player (p1, p2, board) = 
  if player then begin
    if not (can_build_settlement p1) then let () = not_enough_pieces_msg () in false 
    else if not (has_resources_settlement p1) then let () = lack_resources_msg () in false
    else true
  end 
  else begin
    if not (can_build_settlement p2) then let () = not_enough_pieces_msg () in false 
    else if not (has_resources_settlement p2) then let () = lack_resources_msg () in false
    else true
  end

let check_build_city player (p1, p2, board) = 
  if player then begin
    if not (can_build_city p1) then let () = not_enough_pieces_msg () in false 
    else if not (has_resources_city p1) then let () = lack_resources_msg () in false
    else true
  end 
  else begin
    if not (can_build_city p2) then let () = not_enough_pieces_msg () in false 
    else if not (has_resources_city p2) then let () = lack_resources_msg () in false
    else true
  end

let handle_victory player (p1, p2, board) = 
  if player then begin
    if check_victory p1 then GameVictory player
    else return_game (p1, p2, board)
  end 
  else begin 
    if check_victory p2 then GameVictory player
    else return_game (p1, p2, board)
  end 

let rec handle_road (n1, n2) color player (p1, p2, board) = 
  if check_build_road player (p1, p2, board) then begin 
    match add_road n1 n2 player board with
    | Success board' -> 
      let p1', p2' = play_road player (p1, p2) in 
      execute_player player (p1', p2', board')
    | Failure (error, board') -> 
      deal_with_action_error color error;
      execute_player player (p1, p2, board')
  end 
  else execute_player player (p1, p2, board)

and handle_settlement n color player (p1, p2, board) = 
  if check_build_settlement player (p1, p2, board) then begin 
    match add_settlement n player board with
    | Success board' ->  
      let p1', p2' = play_settlement player (p1, p2) in 
      execute_player player (p1', p2', board')
    | Failure (error, board') -> 
      deal_with_action_error color error;
      execute_player player (p1, p2, board')
  end 
  else execute_player player (p1, p2, board)

and handle_city n color player (p1, p2, board) =  
  if check_build_city player (p1, p2, board) then begin
    match add_city n player board with 
    | Success board' ->  
      let p1', p2' = play_city player (p1, p2) in 
      execute_player player (p1', p2', board')
    | Failure (error, board') -> 
      deal_with_action_error color error;
      execute_player player (p1, p2, board')
  end 
  else execute_player player (p1, p2, board)

and handle_marine_trade (start, ending) color player (p1, p2, board) =  
  marine_trade_msg start ending;
  if player then begin
    if not (check_tradeable_4 p1 (cmd_res_to_plyr_res start)) then let () = marine_trade_fail () in execute_player player (p1, p2, board)
    else 
      let p1' = trade_resources_4 p1 (cmd_res_to_plyr_res start) (cmd_res_to_plyr_res ending) in 
      marine_trade_success start ending;
      execute_player player (p1', p2, board)
  end
  else begin
    if not (check_tradeable_4 p2 (cmd_res_to_plyr_res start)) then let () = marine_trade_fail () in execute_player player (p1, p2, board)
    else 
      let p2' = trade_resources_4 p2 (cmd_res_to_plyr_res start) (cmd_res_to_plyr_res ending) in 
      marine_trade_success start ending;
      execute_player player (p1, p2', board)
  end

and handle_two_trade (start, ending) color player (p1, p2, board) =   
  failwith "Unimplemented" 

and handle_three_trade (start, ending) color player (p1, p2, board) =   
  failwith "Unimplemented" 

and handle_player_trade 
    (start, start_amt, ending, ending_amt) color player (p1, p2, board) =  
  failwith "Unimplemented" 

and handle_cheat player (p1, p2, board) = 
  if player then execute_player player (cheat_augment_resources p1, p2, board)
  else execute_player player (p1, cheat_augment_resources p2, board)

and handle_length player (p1, p2, board) = 
  let length = longest_road player board in 
  ANSITerminal.(print_string [green] ("\nYour longest road has length: " ^ string_of_int length ^ ".\n"))

and execute_player player (p1, p2, board) = 
  let color = color_of_player player in 
  player_turn_msg player;
  match () |> read_line |> parse with 
  | Quit -> quit_msg (); GameQuit
  | Length -> handle_length player (p1, p2, board); execute_player player (p1, p2, board)
  | Finish -> handle_victory player (p1, p2, board) (* finish turn and switch to next player *)
  | Invalid -> invalid_msg (); execute_player player (p1, p2, board)
  | Show -> handle_show player (p1, p2, board); execute_player player (p1, p2, board)
  | Buy -> buy_dev_card_msg (); failwith "Unimplemented"
  | Map -> print_map board; execute_player player (p1, p2, board)
  | Cheat -> cheat_msg (); handle_cheat player (p1, p2, board)
  | BuildRoad (n1, n2) -> handle_road (n1, n2) color player (p1, p2, board)
  | BuildSettlement n -> handle_settlement n color player (p1, p2, board)
  | BuildCity n -> handle_city n color player (p1, p2, board)
  | MarineTrade (start, ending) -> 
    handle_marine_trade (start, ending) color player (p1, p2, board)
  | TwoTrade (start, ending) -> handle_two_trade (start, ending) color player (p1, p2, board) 
  | ThreeTrade (start, ending) -> handle_three_trade (start, ending) color player (p1, p2, board) 
  | PlayerTrade (start, start_amt, ending, ending_amt) -> 
    handle_player_trade 
      (start, start_amt, ending, ending_amt) color player (p1, p2, board)

let rec run_game_helper player continue_game = 
  let (p1, p2, board) = handle_roll player continue_game in
  execute_player player (p1, p2, board) |> run_game (not player)

and run_game player game = 
  (run_game_helper player) >>= game

let instantiate_game pregame = 
  pregame
  |> pregame_to_game
  |> run_game true
  |> end_game

