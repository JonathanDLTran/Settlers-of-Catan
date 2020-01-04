type pregame_info = 
  Player.t * Player.t * Board.t

type pregame = 
  | PreContinue of pregame_info
  | PreQuit

val return_game : pregame_info -> pregame
val bind : (pregame_info -> pregame) -> pregame -> pregame
val (>>=) : (pregame_info -> pregame) -> pregame -> pregame
val instantiate_pregame : unit -> pregame
