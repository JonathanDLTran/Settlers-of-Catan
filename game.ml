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

(* ########## GAME UTILITIES ####### *)

let roll () = 
  Random.self_init ();
  let die1 = (Random.int 6) + 1 in 
  Random.self_init ();
  let die2 = (Random.int 6) + 1 in 
  let dice_roll = die1 + die2 in 
  "You rolled a : " ^ string_of_int dice_roll |> print_endline;
  dice_roll

(* ######## GAME ACTIONS ####### *)

let resource_hoard_to_list hoard = 
  [(Player.Wheat, hoard.wheat);
   (Player.Ore, hoard.ore);
   (Player.Wool, hoard.wool);
   (Player.Lumber, hoard.lumber);
   (Player.Brick, hoard.brick);]

let handle_rolling_seven_robber roll player (p1, p2, board) = 
  if roll = 7 then begin
    if player then (set_robber p1 true, set_robber p2 false, board)
    else (set_robber p1 false, set_robber p2 true, board)
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

let rec handle_road (n1, n2) color player (p1, p2, board) = 
  match add_road n1 n2 player board with
  | Success board' -> 
    let p1', p2' = play_road player (p1, p2) in 
    execute_player player (p1', p2', board')
  | Failure (error, board') -> 
    deal_with_action_error color error;
    execute_player player (p1, p2, board')

and handle_settlement n color player (p1, p2, board) = 
  match add_settlement n player board with
  | Success board' ->  
    let p1', p2' = play_settlement player (p1, p2) in 
    execute_player player (p1', p2', board')
  | Failure (error, board') -> 
    deal_with_action_error color error;
    execute_player player (p1, p2, board')

and handle_city n color player (p1, p2, board) =  
  match add_city n player board with 
  | Success board' ->  
    let p1', p2' = play_city player (p1, p2) in 
    execute_player player (p1', p2', board')
  | Failure (error, board') -> 
    deal_with_action_error color error;
    execute_player player (p1, p2, board')

and handle_marine_trade (start, ending) color player (p1, p2, board) =  
  failwith "Unimplemented"

and handle_player_trade 
    (start, start_amt, ending, ending_amt) color player (p1, p2, board) =  
  failwith "Unimplemented" 

and execute_player player (p1, p2, board) = 
  let color = color_of_player player in 
  player_turn_msg player;
  match () |> read_line |> parse with 
  | Quit -> quit_msg (); GameQuit
  | Finish -> return_game (p1, p2, board)  (* finish turn and switch to next player *)
  | Invalid -> invalid_msg (); execute_player player (p1, p2, board)
  | Show -> handle_show player (p1, p2, board); execute_player player (p1, p2, board)
  | Buy -> buy_dev_card_msg (); failwith "Unimplemented"
  | Map -> print_map board; execute_player player (p1, p2, board)
  | BuildRoad (n1, n2) -> handle_road (n1, n2) color player (p1, p2, board)
  | BuildSettlement n -> handle_settlement n color player (p1, p2, board)
  | BuildCity n -> handle_city n color player (p1, p2, board)
  | MarineTrade (start, ending) -> 
    handle_marine_trade (start, ending) color player (p1, p2, board)
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

