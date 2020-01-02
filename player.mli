type player
type t = player

type dev = | Year | Monopoly | Victory
type piece = | City | Settlement | Road
type resource = | Ore | Wheat | Wool | Brick | Lumber

val roll : unit -> int

val initialize_player : player

val resources_to_tuple : player -> (int * int * int * int * int)
val bulk_add_resources : player -> (resource * int) list -> player

val knights_to_list : player -> int list
val add_knight : player -> player
val play_knight : player -> player

val dev_to_tuple : player -> (int * int * int)
val add_dev : dev -> player -> player
val play_dev : dev -> player -> player

val is_longest_road : player -> bool
val is_largest_army : player -> bool
val set_longest_road : player -> bool -> player
val set_largest_army : player -> bool -> player
val get_length_road : player -> int
val set_length_road : player -> int -> player

val get_victory_points : player -> int
val set_victory_points : player -> int -> player
val check_victory : player -> bool

val can_build_road : player -> bool
val can_build_settlement : player -> bool
val can_build_city : player -> bool
val pieces_to_list : player -> int list
val add_piece : player -> piece -> player
val remove_piece : player -> piece -> player

val get_robber : player -> bool
val set_robber : player -> bool -> player

val print_string_of_player_info : player -> unit








