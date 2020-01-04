type board
type t = board

type error = 
  | PostionOccupiedErr
  | AdjacentPositionErr 
  | UnconnectedErr
  | SettlmentMissingErr

type action = 
  | Success of board
  | Failure of error * board

val instantiate_board : board

