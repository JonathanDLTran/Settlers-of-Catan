type game_info = 
  Player.t * Player.t * Board.t

type player = bool

type game = 
  | GameContinue of game_info
  | GameVictory of player
  | GameQuit

val pregame_to_game : Pregame.pregame_info -> game_info
val instantiate_pregame : game_info -> game