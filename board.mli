type board
type t = board

type player = bool

(* ##### GENERATORS ##########*)
type error = 
  | PostionOccupiedErr
  | AdjacentPositionErr 
  | UnconnectedErr
  | SettlmentMissingErr
  | NotAnEdgeErr

type action = 
  | Success of board
  | Failure of error * board

val instantiate_board : board

val add_settlement : int -> player -> board -> action
val add_settlement_pregame : int -> player -> board -> action
val add_city : int -> player -> board -> action
val add_road : int -> int -> player -> board -> action
val add_road_pregame : int -> int -> player -> board -> action

(* ##### Queries ##########*)
type resource_hoard = {
  wheat : int;
  ore : int ;
  wool : int;
  brick : int;
  lumber : int;
}

val get_resources : int -> board -> resource_hoard * resource_hoard

val longest_road : board -> (int * player)
