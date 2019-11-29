type start_resource = string
type start_amt = int
type end_resource = string
type end_amt = int
type structure = string

type command = 
  | Invalid 
  | Show
  | Buy
  | Build of structure
  | MarineTrade of start_resource * end_resource
  | PlayerTrade of start_resource * start_amt * end_resource * end_amt

(* commands are of the form
   Show [resource]
   MarineTrade resource a, resource b
   etc. *)

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

let c_SHOW = "show"
let c_BUILD = "build"
let c_BUY = "buy"
let c_STRUCTURES_LIST = ["city"; "road"; "settlement"]
let c_MARINE = "marinetrade"
let c_PLAYER = "playertrade"
let c_RESOURCES_LIST = ["lumber"; "ore"; "wool"; "brick"; "wheat"]

let string_to_command str_list = 
  match str_list with
  | [] -> Invalid
  | h :: [] ->
    if h = c_SHOW then Show
    else if h = c_BUY then Buy
    else Invalid
  | h1 :: h2 :: [] ->
    if h1 = c_BUILD && List.mem h2 c_STRUCTURES_LIST 
    then Build (h2)
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