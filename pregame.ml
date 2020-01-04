(* Each player places down two settlements and two roads
   not necesarily connected, in order of player 1 places first
   then player 2, then player1, then player2. The distance rule
   must be met, and each settlement must be connected to a
   road, thopugh the settlements and the roads that were plced
   in different turns don't need to be connected *)

open Player
open Board

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

let pregame_command_err_msg () = 
  ANSITerminal.(print_string [green] "The command you entered cannot be used in the pregame phase. ")

let rec place_player_1_settlement_road (p1, p2, board) = 
  failwith "Unimplemented"

let rec place_player_2_settlement_road (p1, p2, board) = 
  failwith "Unimplemented"

let instantiate_pregame () = 
  return_game (initialize_player, initialize_player, instantiate_board)
  |> (>>=) pregame_phase_msg
  |> (>>=) place_player_1_settlement_road
  |> (>>=) place_player_2_settlement_road
  |> (>>=) place_player_1_settlement_road
  |> (>>=) place_player_2_settlement_road
  |> (>>=) end_phase_msg
