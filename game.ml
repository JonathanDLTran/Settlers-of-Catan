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

let bind f pregame = 
  match pregame with 
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

let run_game game = 
  failwith "Unimplemented"



