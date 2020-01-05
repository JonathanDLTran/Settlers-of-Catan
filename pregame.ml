(* Each player places down two settlements and two roads
   not necesarily connected, in order of player 1 places first
   then player 2, then player1, then player2. The distance rule
   must be met, and each settlement must be connected to a
   road, thopugh the settlements and the roads that were plced
   in different turns don't need to be connected *)

open Player
open Board
open Command

type pregame_info = 
  Player.t * Player.t * Board.t

type pregame = 
  | PreContinue of pregame_info
  | PreQuit

let return_game (p1, p2, board) = 
  PreContinue (p1, p2, board)

let bind f pregame = 
  match pregame with 
  | PreQuit -> PreQuit
  | PreContinue p -> f p

let (>>=) = bind

let pregame_phase_msg pregame = 
  ANSITerminal.(print_string [green] "This is the pregame phase. ");
  ANSITerminal.(print_string [green] "Player 1 will place one settlement and a road connected to that settlement. ");
  ANSITerminal.(print_string [green] "Player 2 will place one settlement and a road connected to that settlement. ");
  ANSITerminal.(print_string [green] "Then each player will place one more road and settlement each. ");
  PreContinue pregame

let end_phase_msg pregame = 
  ANSITerminal.(print_string [green] "The pregame phase is complete. ");
  PreContinue pregame

let place_settlement_msg color = 
  ANSITerminal.(print_string [color] "Please place a settlement. Enter an integer corresponding
  to a node on the map. ")

let place_road_msg color = 
  ANSITerminal.(print_string [color] "Please place a road. Enter two integers corresponding
  to connected nodes on the map. ") 

let pregame_command_err_msg () = 
  ANSITerminal.(print_string [green] "The command you entered cannot be used in the pregame phase. ");
  ANSITerminal.(print_string [green] "Try again, or type quit to quit. ")

let pregame_quit_msg () = 
  ANSITerminal.(print_string [green] "Quitting game. ")

let deal_with_action_error color error = 
  match error with 
  | PostionOccupiedErr -> ANSITerminal.(print_string [color] "Position was occupied . ")
  | AdjacentPositionErr ->  ANSITerminal.(print_string [color] "You cannot place a structure like a settlement or a city adjacent to another structure. ")
  | UnconnectedErr -> ANSITerminal.(print_string [color] "You must place a road, settlement or city connected to a road of yours. ")
  | SettlmentMissingErr -> ANSITerminal.(print_string [color] "The city at the location you wish to place must have a settlement to upgrade already. ")
  | NotAnEdgeErr ->  ANSITerminal.(print_string [color] "The nodes you specified are not an edge on the board. ")

let color_of_player player = 
  if player then ANSITerminal.blue
  else ANSITerminal.red

let rec place_player_road player (p1, p2, board) = 
  let color = color_of_player player in 
  place_road_msg color;
  match () |> read_line |> parse with 
  | BuildRoad (n1, n2) -> begin
      match add_road_pregame n1 n2 player board with
      | Success board' -> return_game (p1, p2, board')
      | Failure (error, board') -> 
        deal_with_action_error color error;
        place_player_road player (p1, p2, board')
    end 
  | Quit -> 
    pregame_quit_msg ();
    PreQuit
  | _ -> 
    pregame_command_err_msg () ;
    place_player_road player (p1, p2, board)

let rec place_player_settlement player (p1, p2, board) = 
  let color = color_of_player player in 
  place_settlement_msg color;
  match () |> read_line |> parse with 
  | BuildSettlement n -> begin
      match add_settlement_pregame n player board with
      | Success board' -> return_game (p1, p2, board')
      | Failure (error, board') -> 
        deal_with_action_error color error;
        place_player_settlement player (p1, p2, board')
    end 
  | Quit -> 
    pregame_quit_msg ();
    PreQuit
  | _ -> 
    pregame_command_err_msg () ;
    place_player_settlement player (p1, p2, board)

let place_player_settlement_road player (p1, p2, board) = 
  place_player_settlement player (p1, p2, board)
  |> (>>=) (place_player_road player)

let place_player_1_settlement_road (p1, p2, board) = 
  place_player_settlement_road true (p1, p2, board)

let place_player_2_settlement_road (p1, p2, board) = 
  place_player_settlement_road false (p1, p2, board)

let instantiate_pregame () = 
  return_game (initialize_player, initialize_player, instantiate_board)
  |> (>>=) pregame_phase_msg
  |> (>>=) place_player_1_settlement_road
  |> (>>=) place_player_2_settlement_road
  |> (>>=) place_player_1_settlement_road
  |> (>>=) place_player_2_settlement_road
  |> (>>=) end_phase_msg
