type start_resource = string
type start_amt = int
type end_resource = string
type end_amt = int
type structure = string

(** [command] represents a command by the player.
    [Quit] ends the game. 
    [Invalid] is an invalid command.
    [Show] reveals the player's hand : all the cards, plsu dev cards, and remaining
    cities, settlements and roads. 
    [Buy] buys the top dev card in the pile. 
    [Finish] ends the turn for the player.
    [Build structure location] builds the structure at the given
    location. 
    [Marine Trade start end] trades 4 of the start resource for the end resource. 
    [Player trade start start_amount end end_amount] trades start_amount of start
    for end_amount of end.  *)
type command = 
  | Quit
  | Invalid 
  | Show
  | Buy
  | Finish
  | Map
  | Cheat
  | Length
  | BuildRoad of int * int 
  | BuildSettlement of int 
  | BuildCity of int 
  | MarineTrade of start_resource * end_resource
  | ThreeTrade of start_resource * end_resource
  | TwoTrade of start_resource * end_resource
  | PlayerTrade of start_resource * start_amt * end_resource * end_amt

let is_alpha c = 
  match c with
  | 'a' .. 'z' -> true
  | _ -> false

let is_digit c = 
  match c with
  | '0' .. '9' -> true
  | _ -> false

let rec string_is_digit s = 
  match s.[0] with
  | exception (Invalid_argument _) -> true
  | c -> is_digit c && string_is_digit (String.sub s 1 ((String.length s) - 1))

let c_QUIT = "quit"
let c_SHOW = "show"
let c_BUILDROAD = "build_road"
let c_BUILDSETTLEMENT = "build_settlement"
let c_BUILDCITY = "build_city"
let c_BUY = "buy"
let c_STRUCTURES_LIST = ["city"; "road"; "settlement"]
let c_MARINE = "marinetrade"
let c_PLAYER = "playertrade"
let c_TWOTRADE = "twotrade"
let c_THREETRADE = "threetrade"
let c_RESOURCES_LIST = ["lumber"; "ore"; "wool"; "brick"; "wheat"]
let c_FINISH = "finish"
let c_MAP = "map"
let c_CHEAT = "cheat"
let c_LENGTH = "length"

let string_to_command str_list = 
  match str_list with
  | [] -> Invalid
  | h :: [] ->
    if h = c_QUIT then Quit
    else if h = c_SHOW then Show
    else if h = c_BUY then Buy
    else if h = c_FINISH then Finish
    else if h = c_MAP then Map
    else if h = c_CHEAT then Cheat
    else if h = c_LENGTH then Length
    else Invalid
  | h1 :: h2 :: [] when h1 = c_BUILDSETTLEMENT ->
    if string_is_digit h2 
    then BuildSettlement (int_of_string h2)
    else Invalid
  | h1 :: h2 :: [] ->
    if h1 = c_BUILDCITY && string_is_digit h2 
    then BuildCity (int_of_string h2)
    else Invalid
  | h1 :: h2 :: h3 :: [] when h1 = c_BUILDROAD ->
    if string_is_digit h2 && string_is_digit h3 
    then BuildRoad (int_of_string h2, int_of_string h3)
    else Invalid 
  | h1 :: h2 :: h3 :: [] when h1 = c_TWOTRADE ->
    if List.mem h2 c_RESOURCES_LIST 
    && List.mem h3 c_RESOURCES_LIST 
    then TwoTrade (h2, h3)
    else Invalid
  | h1 :: h2 :: h3 :: [] when h1 = c_THREETRADE ->
    if List.mem h2 c_RESOURCES_LIST 
    && List.mem h3 c_RESOURCES_LIST 
    then ThreeTrade (h2, h3)
    else Invalid
  | h1 :: h2 :: h3 :: [] ->
    if h1 = c_MARINE 
    && List.mem h2 c_RESOURCES_LIST 
    && List.mem h3 c_RESOURCES_LIST 
    then MarineTrade (h2, h3)
    else Invalid
  | h1 :: h2 :: h3 :: h4 :: h5 :: [] ->
    if h1 = c_PLAYER
    && List.mem h2 c_RESOURCES_LIST 
    && List.mem h4 c_RESOURCES_LIST 
    && String.length h3 > 0 
    && String.length h5 > 0 
    && string_is_digit h3
    && string_is_digit h5 
    then PlayerTrade (h2, int_of_string h3, h4, int_of_string h5)
    else Invalid
  | _ -> Invalid

let clean s = 
  s 
  |> String.lowercase_ascii
  |> String.trim 
  |> String.split_on_char ' '
  |> List.filter (fun elt -> elt <> "")

let parse s = 
  s |> clean |> string_to_command

type affirmation = 
  | NotAffirmative 
  | Accept 
  | Reject 

let c_ACCEPT = "accept"
let c_REJECT = "reject"

let string_to_affirmation s = 
  match s with 
  | str :: [] -> 
    if str = c_ACCEPT then Accept
    else if str = c_REJECT then Reject
    else NotAffirmative
  | _ -> NotAffirmative

let parse_affirmative s = 
  s |> clean |> string_to_affirmation

type robber = 
  | NotLocation
  | NewLocation of string

let string_to_tile s = 
  match s with 
  | str :: [] ->
    if String.length str > 1 then  NotLocation 
    else 
      let c = String.get str 0 in 
      if is_alpha c && c >= 'a' && c <= 's' then NewLocation (str)
      else NotLocation
  | _ -> NotLocation

let parse_robber s = 
  s |> clean |> string_to_tile