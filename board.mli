type board
type t = board

type player = bool

type resource = 
  | Wheat
  | Ore 
  | Wool 
  | Lumber 
  | Brick
  | Desert


(* ###### CONVERSIONS ######## *)

val string_to_resource : string -> resource

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

val longest_road : player -> board -> int

val print_map : board -> unit

val get_robber_tile : board -> string

val can_robber_steal_card : player -> board -> bool

val check_player_three_to_one : player -> board -> bool

val check_player_two_to_one : player -> string -> board -> bool

(* ###### MANIPULATOR ####### *)

val set_robber_tile : char -> board -> board 


(* EXTRA DEV CARDS *)

type dev = 
  | Knight
  | Road
  | Year
  | Monopoly
  | Victory

val string_to_dev : string -> dev
val dev_to_string : dev -> string

type dev_deck = dev list

val init_dev_deck : dev_deck

val is_dev_empty : board -> bool
val get_dev : board -> (dev * board)