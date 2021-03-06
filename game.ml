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
  ANSITerminal.(print_string [green] ("Attempting marine trade with four " ^ res1 ^ " for one " ^ res2 ^ ".\n"))

let marine_trade_success res1 res2 = 
  ANSITerminal.(print_string [green] ("Successfully traded four " ^ res1 ^ " for one " ^ res2 ^ ".\n"))

let marine_trade_fail () = 
  ANSITerminal.(print_string [green] "Marine trade failed. ")

let three_to_one_trade_msg res1 res2 = 
  ANSITerminal.(print_string [green] ("Attempting three to one trade with three " ^ res1 ^ " for one " ^ res2 ^ ".\n"))

let three_to_one_success_msg res1 res2 = 
  ANSITerminal.(print_string [green] ("Successfully traded three " ^ res1 ^ " for one " ^ res2 ^ ".\n"))

let three_to_one_no_port_msg () = 
  ANSITerminal.(print_string [green] "You have no three to one conversion port. \n")

let insufficient_resouces_msg () = 
  ANSITerminal.(print_string [green] "You lack enough of the appropriate resources to trade. \n")

let two_to_one_trade_msg res1 res2 = 
  ANSITerminal.(print_string [green] ("Attempting two to one trade with two " ^ res1 ^ " for one " ^ res2 ^ ".\n"))

let two_to_one_success_msg res1 res2 = 
  ANSITerminal.(print_string [green] ("Successfully traded two " ^ res1 ^ " for one " ^ res2 ^ ".\n"))

let two_to_one_no_port_msg res1 = 
  ANSITerminal.(print_string [green] ("You have no two to one conversion port for " ^ res1 ^ " . \n"))

let player_trade_msg amt1 res1 amt2 res2 = 
  ANSITerminal.(print_string [green] ("You are attempting a player trade with " ^ string_of_int amt1 ^ " " ^ res1 ^ " for " ^ string_of_int amt2 ^ " " ^ res2))

let opponent_insufficient_resources () = 
  ANSITerminal.(print_string [green] "Your opponent lacks enough of the appropriate resources to trade. \n")

let player_trade_success amt1 res1 amt2 res2 = 
  ANSITerminal.(print_string [green] ("You successfully finished a player trade with " ^ string_of_int amt1 ^ " " ^ res1 ^ " for " ^ string_of_int amt2 ^ " " ^ res2))

let opponent_trade_msg amt1 res1 amt2 res2 = 
  ANSITerminal.(print_string [green] ("\nPass the computer\n; Opponent: Please accept or reject this trade offer for " ^ string_of_int amt1 ^ " " ^ res1 ^ " for " ^ string_of_int amt2 ^ " " ^ res2 ^ "\n."))

let opponent_reject () = 
  ANSITerminal.(print_string [green] "Your opponent rejected the trade")

let trade_zero_things () = 
  ANSITerminal.(print_string [green] "You cannot trade 0 resources for other resources or ask for 0 resources")

let lack_resources_dev_msg () = 
  ANSITerminal.(print_string [green] "You do not have enough resources to buy a dev card.")

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

let largest_army player (p1, p2, board) = 
  let num_knights1 = num_knights p1 in 
  let num_knights2 = num_knights p2 in 
  let vp1 = get_victory_points p1 in 
  let vp2 = get_victory_points p2 in 

  if num_knights1 > 2 && num_knights2 > 2 && num_knights1 > num_knights2 
  then (set_victory_points p1 (vp1 + 2), p2, board)
  else if num_knights1 > 2 && num_knights2 > 2 && num_knights1 < num_knights2 
  then (p1, set_victory_points p2 (vp2 + 2), board)
  else (p1, p2, board)

let handle_victory player (p1, p2, board) = 
  if player then begin
    if check_victory p1 then GameVictory player
    else 
      let p1' = reset_dev_current p1 in 
      return_game (p1', p2, board)
  end 
  else begin 
    if check_victory p2 then GameVictory player
    else 
      let p2' = reset_dev_current p2 in 
      return_game (p1, p2', board)
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
  two_to_one_trade_msg start ending;
  if player then begin 
    if not (can_remove_resource 2 start p1) then let () = insufficient_resouces_msg () in execute_player player (p1, p2, board)
    else if not (check_tradeable_2 p1 (cmd_res_to_plyr_res start)) then let () = insufficient_resouces_msg () in execute_player player (p1, p2, board)
    else if not (check_player_two_to_one player start board) then let () = two_to_one_no_port_msg start in execute_player player (p1, p2, board)
    else let p1' = trade_resources_2 p1 (cmd_res_to_plyr_res start) (cmd_res_to_plyr_res ending) in 
      two_to_one_success_msg start ending;
      execute_player player (p1', p2, board)
  end 
  else begin
    if not (can_remove_resource 2 start p2) then let () = insufficient_resouces_msg () in execute_player player (p1, p2, board)
    else if not (check_tradeable_2 p2 (cmd_res_to_plyr_res start)) then let () = insufficient_resouces_msg () in execute_player player (p1, p2, board)
    else if not (check_player_two_to_one player start board) then let () = two_to_one_no_port_msg start in execute_player player (p1, p2, board)
    else let p2' = trade_resources_2 p2 (cmd_res_to_plyr_res start) (cmd_res_to_plyr_res ending ) in 
      two_to_one_success_msg start ending;
      execute_player player (p1, p2', board)
  end

and handle_three_trade (start, ending) color player (p1, p2, board) =   
  three_to_one_trade_msg start ending;
  if player then begin 
    if not (can_remove_resource 3 start p1) then let () = insufficient_resouces_msg () in execute_player player (p1, p2, board)
    else if not (check_tradeable_3 p1 (cmd_res_to_plyr_res start)) then let () = insufficient_resouces_msg () in execute_player player (p1, p2, board)
    else if not (check_player_three_to_one player board) then let () = three_to_one_no_port_msg () in execute_player player (p1, p2, board)
    else let p1' = trade_resources_3 p1 (cmd_res_to_plyr_res start) (cmd_res_to_plyr_res ending ) in 
      three_to_one_success_msg start ending;
      execute_player player (p1', p2, board)
  end 
  else begin
    if not (can_remove_resource 3 start p2) then let () = insufficient_resouces_msg () in execute_player player (p1, p2, board)
    else if not (check_tradeable_3 p2 (cmd_res_to_plyr_res start)) then let () = insufficient_resouces_msg () in execute_player player (p1, p2, board)
    else if not (check_player_three_to_one player board) then let () = three_to_one_no_port_msg () in execute_player player (p1, p2, board)
    else let p2' = trade_resources_3 p2 (cmd_res_to_plyr_res start) (cmd_res_to_plyr_res ending ) in 
      three_to_one_success_msg start ending;
      execute_player player (p1, p2', board)
  end

and handle_player_trade 
    (start, start_amt, ending, ending_amt) color player (p1, p2, board) =  
  player_trade_msg start_amt start ending_amt ending;
  if start_amt = 0 || ending_amt = 0 then let () = trade_zero_things () in execute_player player (p1, p2, board) 
  else begin 
    if player then begin 
      if not (can_remove_resource start_amt start p1) then let () = insufficient_resouces_msg () in execute_player player (p1, p2, board)
      else 
        let () = opponent_trade_msg start_amt start ending_amt ending in 
        match () |> read_line |> parse_affirmative with 
        | NotAffirmative -> opponent_reject (); execute_player player (p1, p2, board)
        | Reject -> opponent_reject (); execute_player player (p1, p2, board)
        | Accept -> begin 
            if not (can_remove_resource ending_amt start p2) then let () = opponent_insufficient_resources () in execute_player player (p1, p2, board)
            else 
              let p1' = player_trade_resources p1 start start_amt ending ending_amt in 
              let p2' = player_trade_resources p2 ending ending_amt start start_amt in 
              player_trade_success start_amt start ending_amt ending;
              execute_player player (p1', p2', board)
          end 
    end 
    else 
    if not (can_remove_resource start_amt start p2) then let () = insufficient_resouces_msg () in execute_player player (p1, p2, board)
    else 
      let () = opponent_trade_msg start_amt start ending_amt ending in 
      match () |> read_line |> parse_affirmative with 
      | NotAffirmative -> opponent_reject (); execute_player player (p1, p2, board)
      | Reject -> opponent_reject (); execute_player player (p1, p2, board)
      | Accept -> begin 
          if not (can_remove_resource ending_amt start p2) then let () = opponent_insufficient_resources () in execute_player player (p1, p2, board)
          else 
            let p2' = player_trade_resources p2 start start_amt ending ending_amt in 
            let p1' = player_trade_resources p1 ending ending_amt start start_amt in 
            player_trade_success start_amt start ending_amt ending;
            execute_player player (p1', p2', board)
        end 
  end    

and handle_cheat player (p1, p2, board) = 
  if player then execute_player player (cheat_augment_resources p1, p2, board)
  else execute_player player (p1, cheat_augment_resources p2, board)

and handle_length player (p1, p2, board) = 
  let length = longest_road player board in 
  ANSITerminal.(print_string [green] ("\nYour longest road has length: " ^ string_of_int length ^ ".\n"))

and handle_buy player (p1, p2, board) = 
  if player then begin
    if not (has_resources_dev p1) then let () = lack_resources_dev_msg () in execute_player player (p1, p2, board)
    else 
      let (dev, board') = get_dev board in
      let p1' = (dev |> Board.dev_to_string |> Player.string_to_dev |> add_dev) p1 in 
      ANSITerminal.(print_string [green] ("You bought a " ^ (dev |> Board.dev_to_string) ^ " card."));
      execute_player player (p1', p2, board')
  end
  else begin
    if not (has_resources_dev p2) then let () = lack_resources_dev_msg () in execute_player player (p1, p2, board)
    else
      let (dev, board') = get_dev board in
      let p2' = (dev |> Board.dev_to_string |> Player.string_to_dev |> add_dev) p2 in 
      ANSITerminal.(print_string [green] ("You bought a " ^ (dev |> Board.dev_to_string) ^ " card."));
      execute_player player (p1, p2', board')
  end

and handle_monopoly_resource player p1 p2 = 
  if player then begin 
    ANSITerminal.(print_string [green] "Please enter a resource: ");
    match () |> read_line |> parse_resource with 
    | Lumber -> 
      let n_res = get_num_resource p2 (Player.Lumber) in 
      (add_resource_n n_res p1 Player.Lumber, remove_resource_n n_res p2 Player.Lumber)
    | Ore -> 
      let n_res = get_num_resource p2 (Player.Ore) in 
      (add_resource_n n_res p1 Player.Ore, remove_resource_n n_res p2 Player.Ore)
    | Wool -> 
      let n_res = get_num_resource p2 (Player.Wool) in 
      (add_resource_n n_res p1 Player.Wool, remove_resource_n n_res p2 Player.Wool)
    | Wheat -> 
      let n_res = get_num_resource p2 (Player.Wheat) in 
      (add_resource_n n_res p1 Player.Wheat, remove_resource_n n_res p2 Player.Wheat)
    | Brick -> 
      let n_res = get_num_resource p2 (Player.Brick) in 
      (add_resource_n n_res p1 Player.Brick, remove_resource_n n_res p2 Player.Brick)
    | NotResource -> handle_monopoly_resource player p1 p2
  end 
  else begin 
    ANSITerminal.(print_string [green] "Please enter a resource: ");
    match () |> read_line |> parse_resource with 
    | Lumber -> 
      let n_res = get_num_resource p1 (Player.Lumber) in 
      (remove_resource_n n_res p1 Player.Lumber, add_resource_n n_res p2 Player.Lumber)
    | Ore -> 
      let n_res = get_num_resource p1 (Player.Ore) in 
      (remove_resource_n n_res p1 Player.Ore, add_resource_n n_res p2 Player.Ore)
    | Wool -> 
      let n_res = get_num_resource p1 (Player.Wool) in 
      (remove_resource_n n_res p1 Player.Wool, add_resource_n n_res p2 Player.Wool)
    | Wheat -> 
      let n_res = get_num_resource p1 (Player.Wheat) in 
      (remove_resource_n n_res p1 Player.Wheat, add_resource_n n_res p2 Player.Wheat)
    | Brick -> 
      let n_res = get_num_resource p1 (Player.Brick) in 
      (remove_resource_n n_res p1 Player.Brick, add_resource_n n_res p2 Player.Brick)
    | NotResource -> handle_monopoly_resource player p1 p2
  end

and handle_play_monopoly player (p1, p2, board) = 
  if player then begin 
    if not (has_dev (Player.string_to_dev "monopoly") p1) 
    then let () = ANSITerminal.(print_string [green] "\nYou do not have a monopoly card you can currently play. \n") in execute_player player (p1, p2, board)
    else 
      let () = ANSITerminal.(print_string [green] "\nPlaying a monopoly. \n") in 
      let p1', p2' = handle_monopoly_resource player p1 p2  in 
      execute_player player ((p1', p2', board) |> handle_one_dev_card player)
  end
  else begin 
    if not (has_dev (Player.string_to_dev "monopoly") p2) 
    then let () = ANSITerminal.(print_string [green] "\nYou do not have a monopoly card you can currently play. \n") in execute_player player (p1, p2, board)
    else 
      let () = ANSITerminal.(print_string [green] "\nPlaying a monopoly. \n") in 
      let p1', p2' = handle_monopoly_resource player p1 p2  in 
      execute_player player ((p1', p2', board) |> handle_one_dev_card player)
  end

and handle_play_victory player (p1, p2, board) = 
  if player then begin 
    if not (has_dev (Player.string_to_dev "victory") p1) 
    then let () = ANSITerminal.(print_string [green] "\nYou do not have a victory card you can currently play. \n") in execute_player player (p1, p2, board)
    else if get_victory_points p1 <> 9 
    then let () = ANSITerminal.(print_string [green] "\nYou can only play a victory card when you have nine victory points. \n") in execute_player player (p1, p2, board)
    else 
      let () = ANSITerminal.(print_string [green] "\nPlaying a victory card. \n") in 
      let p1' = play_dev (Player.string_to_dev "victory") p1 in 
      execute_player player ((p1', p2, board) |> handle_one_dev_card player)
  end
  else begin 
    if not (has_dev (Player.string_to_dev "victory") p2) 
    then let () = ANSITerminal.(print_string [green] "\nYou do not have a victory card you can currently play. \n") in execute_player player (p1, p2, board)
    else if get_victory_points p2 <> 9 
    then let () = ANSITerminal.(print_string [green] "\nYou can only play a victory card when you have nine victory points. \n") in execute_player player (p1, p2, board)
    else 
      let () = ANSITerminal.(print_string [green] "\nPlaying a victory card. \n") in 
      let p2' = play_dev (Player.string_to_dev "victory") p2 in 
      execute_player player ((p1, p2', board) |> handle_one_dev_card player)
  end

and handle_build_road_helper (n1, n2) player p1 p2 board = 
  if check_build_road player (p1, p2, board) then begin 
    match add_road n1 n2 player board with
    | Success board' -> 
      let p1', p2' = play_road player (p1, p2) in 
      (p1', p2' , board')
    | Failure (error, board') -> 
      deal_with_action_error ANSITerminal.red error;
      handle_build_road player p1 p2 board'

  end 
  else handle_build_road player p1 p2 board

and handle_build_road player p1 p2 board = 
  ANSITerminal.(print_string [green] "Please enter a road: ");
  match () |> read_line |> parse with 
  | BuildRoad (n1, n2) -> handle_build_road_helper (n1, n2) player p1 p2 board  
  | _  -> handle_build_road player p1 p2 board

and handle_play_road player (p1, p2, board) = 
  if player then begin 
    if not (has_dev (Player.string_to_dev "road") p1) 
    then let () = ANSITerminal.(print_string [green] "\nYou do not have a build road card you can currently play. \n") in execute_player player (p1, p2, board)
    else 
      let () = ANSITerminal.(print_string [green] "\nPlaying a build road. \n") in 
      let p1', p2', board' = handle_build_road player p1 p2 board in 
      let p1', p2', board' = handle_build_road player p1' p2' board' in 
      execute_player player ((p1', p2', board') |> handle_one_dev_card player)
  end
  else begin 
    if not (has_dev (Player.string_to_dev "road") p2) 
    then let () = ANSITerminal.(print_string [green] "\nYou do not have a build roadcard you can currently play. \n") in execute_player player (p1, p2, board)
    else 
      let () = ANSITerminal.(print_string [green] "\nPlaying a build road. \n") in 
      let p1', p2', board' = handle_build_road player p1 p2 board in 
      let p1', p2', board' = handle_build_road player p1' p2' board' in 
      execute_player player ((p1', p2', board') |> handle_one_dev_card player)
  end

and handle_get_resource p = 
  ANSITerminal.(print_string [green] "Please enter a resource: ");
  match () |> read_line |> parse_resource with 
  | Lumber -> add_resource_n 1 p (Player.Lumber)
  | Ore -> add_resource_n 1 p (Player.Ore)
  | Wool -> add_resource_n 1 p (Player.Wool)
  | Wheat -> add_resource_n 1 p (Player.Wheat)
  | Brick -> add_resource_n 1 p (Player.Brick)
  | NotResource -> handle_get_resource p

and handle_play_year player (p1, p2, board) = 
  if player then begin 
    if not (has_dev (Player.string_to_dev "year") p1) 
    then let () = ANSITerminal.(print_string [green] "\nYou do not have a year of plenty card you can currently play. \n") in execute_player player (p1, p2, board)
    else 
      let () = ANSITerminal.(print_string [green] "\nPlaying a year of plenty. \n") in 
      let p1' = p1 |> handle_get_resource |> handle_get_resource in 
      execute_player player ((p1', p2, board) |> handle_one_dev_card player)
  end
  else begin 
    if not (has_dev (Player.string_to_dev "year") p2) 
    then let () = ANSITerminal.(print_string [green] "\nYou do not have a year of plenty card you can currently play. \n") in execute_player player (p1, p2, board)
    else 
      let () = ANSITerminal.(print_string [green] "\nPlaying a year of plenty. \n") in 
      let p2' = p2 |> handle_get_resource |> handle_get_resource in 
      execute_player player ((p1, p2', board) |> handle_one_dev_card player)
  end

and handle_play_knight player (p1, p2, board) = 
  if player then begin 
    if not (has_dev (Player.string_to_dev "knight") p1) 
    then let () = ANSITerminal.(print_string [green] "\nYou do not have a knight you can currently play. \n") in execute_player player (p1, p2, board)
    else 
      let () = ANSITerminal.(print_string [green] "\nPlaying a knight. \n") in 
      let p1' = play_dev (Player.string_to_dev "knight") p1 in 
      execute_player player ((p1', p2, board) |> handle_one_dev_card player)
  end
  else begin 
    if not (has_dev (Player.string_to_dev "knight") p2) 
    then let () = ANSITerminal.(print_string [green] "\nYou do not have a knight you can currently play. \n") in execute_player player (p1, p2, board)
    else 
      let () = ANSITerminal.(print_string [green] "\nPlaying a knight. \n") in 
      let p2' = play_dev (Player.string_to_dev "knight") p2 in 
      execute_player player ((p1, p2', board) |> handle_one_dev_card player)
  end

and handle_play_once card player (p1, p2, board) = 
  if player then begin 
    if has_dev_played p1 then true
    else false
  end
  else begin 
    if has_dev_played p2 then true
    else false
  end

and handle_play_card card player (p1, p2, board) = 
  if (handle_play_once card player (p1, p2, board))
  then execute_player player (p1, p2, board)
  else
    match card with 
    | "monopoly" -> handle_play_monopoly player (p1, p2, board)
    | "victory" -> handle_play_victory player (p1, p2, board)
    | "road" -> handle_play_road player (p1, p2, board) 
    | "year" -> handle_play_year player (p1, p2, board)
    | "knight" -> handle_play_knight player (p1, p2, board)
    | _ -> failwith "In Game, cannot be non card"

and handle_one_dev_card player (p1, p2, board) = 
  if player then (add_dev_card_played_already p1, p2, board)
  else (p1, add_dev_card_played_already p2, board)

and execute_player player (p1, p2, board) = 
  let color = color_of_player player in 
  player_turn_msg player;
  match () |> read_line |> parse with 
  | Quit -> quit_msg (); GameQuit
  | Length -> handle_length player (p1, p2, board); execute_player player (p1, p2, board)
  | Finish -> handle_victory player ((p1, p2, board) |> largest_army player) (* finish turn and switch to next player *)
  | Invalid -> invalid_msg (); execute_player player (p1, p2, board)
  | Show -> handle_show player (p1, p2, board); execute_player player (p1, p2, board)
  | Buy -> buy_dev_card_msg (); handle_buy player (p1, p2, board)
  | Play card -> handle_play_card card player (p1, p2, board)
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

