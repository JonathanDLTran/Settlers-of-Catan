type dev = 
  | Knight
  | Road
  | Year
  | Monopoly
  | Victory

val string_to_dev : string -> dev
val dev_to_string : dev -> string

type dev_deck = dev list
type t = dev_deck

val init_dev_deck : dev_deck

val is_dev_empty : dev_deck -> bool
val hd_dev : dev_deck -> dev
val tl_dev : dev_deck -> dev_deck