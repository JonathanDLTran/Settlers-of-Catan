type player
type t = player

type dev = | Year | Monopoly | Victory
type piece = | City | Settlement | Road
type resource = | Ore | Wheat | Wool | Brick | Lumber

val cheat_augment_resources : player -> player

val roll : unit -> int

val initialize_player : player

val resources_to_tuple : player -> (int * int * int * int * int)
val bulk_add_resources : player -> (resource * int) list -> player
val bulk_remove_resources : player -> (resource * int) list -> player

val knights_to_list : player -> int list
val add_knight : player -> player
val play_knight : player -> player

val dev_to_tuple : player -> (int * int * int)
val has_resources_dev : player -> bool
val add_dev : dev -> player -> player
val play_dev : dev -> player -> player
val add_dev_card_played_already : player -> player
val reset_dev_played : player -> player

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
val has_resources_road : player -> bool
val can_build_settlement : player -> bool
val has_resources_settlement : player -> bool
val can_build_city : player -> bool
val has_resources_city : player -> bool
val pieces_to_list : player -> int list
val add_piece : player -> piece -> player
val add_piece_pregame : player -> piece -> player
val remove_piece : player -> piece -> player

val get_robber : player -> bool
val set_robber : player -> bool -> player
val halve_hand : player -> player
val randomly_remove_cards : int -> player -> player
val robber_choose_random_card : player -> resource option

val check_tradeable_n : int -> player -> resource -> bool
val trade_resources_n : int -> int -> player -> resource -> resource -> player
val check_tradeable_4 : player -> resource -> bool
val trade_resources_4 : player -> resource -> resource -> player
val check_tradeable_3 : player -> resource -> bool
val trade_resources_3 : player -> resource -> resource -> player
val check_tradeable_2 : player -> resource -> bool
val trade_resources_2 : player -> resource -> resource -> player

val print_string_of_player_info : player -> unit








