type game_info = 
  Player.t * Player.t * Board.t

type player = bool

type game = 
  | GameContinue of game_info
  | GameVictory of player
  | GameQuit

val pregame_to_game : Pregame.pregame -> game
val run_game : bool -> game -> game
val end_game : game -> unit
val instantiate_game : Pregame.pregame -> unit
