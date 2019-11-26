let c_BRICK = 19
let c_GRAIN = 19
let c_LUMBER = 19
let c_ORE = 19
let c_WOOL = 19
let c_KNIGHT = 14
let c_ROAD_BUILD = 2
let c_YEAR_PLENTY = 2
let c_MONOPOLY = 2
let c_VICTORY = 5
let c_NUM_DEV = 
  c_KNIGHT + c_ROAD_BUILD + c_YEAR_PLENTY + c_MONOPOLY + c_VICTORY 

type resource_deck = {
  brick : int;
  grain : int;
  lumber : int;
  ore : int;
  wool : int;
}

type development_deck = {
  knight : int;
  road_building : int;
  year_of_plenty : int;
  monopoly : int;
  victory : int;
  num_dev : int;
}

let init_resource_deck = {
  brick = c_BRICK;
  grain = c_GRAIN;
  lumber = c_LUMBER;
  ore = c_ORE;
  wool = c_WOOL;
}

let init_development_deck = {
  knight = c_KNIGHT;
  road_building = c_ROAD_BUILD;
  year_of_plenty = c_YEAR_PLENTY;
  monopoly = c_MONOPOLY;
  victory = c_VICTORY;
  num_dev = c_NUM_DEV;
}

type resource = 
  | Brick
  | Grain
  | Lumber
  | Ore
  | Wool

let resource_of_string str = 
  if str = "brick" then Brick
  else if str = "grain" then Grain
  else if str = "lumber" then Lumber
  else if str = "ore" then Ore
  else if str = "wool" then Wool
  else failwith "Precondition violated : cannot give non resource"

(** [get_resource resource)deck resource_string] is the new resource
    deck with one less of the resource given by resource_string,
    if there is enough of that resource in the deck, else None.  *)
let get_resource resource_deck resource_string = 
  let resource = resource_of_string resource_string in 
  match resource with 
  | Brick -> 
    let num_bricks = resource_deck.brick in 
    if num_bricks = 0 then None
    else Some {resource_deck with brick = num_bricks - 1}
  | Grain -> 
    let num_grain = resource_deck.grain in 
    if num_grain = 0 then None
    else Some {resource_deck with grain = num_grain - 1}
  | Lumber -> 
    let num_wood = resource_deck.lumber in 
    if num_wood = 0 then None
    else Some {resource_deck with lumber = num_wood - 1}
  | Ore -> 
    let num_ore = resource_deck.ore in 
    if num_ore = 0 then None
    else Some {resource_deck with ore = num_ore - 1}
  | Wool ->
    let num_wool = resource_deck.wool in 
    if num_wool = 0 then None
    else Some {resource_deck with wool = num_wool - 1}

(** [add_resource deck resource] adds the resource back to the deck. *)
let add_resource resource_deck resource_string = 
  let resource = resource_of_string resource_string in 
  match resource with 
  | Brick -> 
    {resource_deck with brick = resource_deck.brick + 1}
  | Grain -> 
    {resource_deck with grain = resource_deck.grain + 1}
  | Lumber -> 
    {resource_deck with lumber = resource_deck.lumber + 1}
  | Ore -> 
    {resource_deck with ore = resource_deck.ore + 1}
  | Wool ->
    {resource_deck with wool = resource_deck.wool + 1}

let length_dev_deck dev_deck = 
  [dev_deck.knight;
   dev_deck.road_building + dev_deck.knight;
   dev_deck.year_of_plenty + dev_deck.road_building + dev_deck.knight;
   dev_deck.monopoly + dev_deck.year_of_plenty + dev_deck.road_building + dev_deck.knight;
   dev_deck.victory + dev_deck.monopoly + dev_deck.year_of_plenty + dev_deck.road_building + dev_deck.knight;]

type dev = 
  | Knight
  | Road
  | Year
  | Monopoly
  | Victory

(** [prob_choose_dev n dev_deck] chooses the dev card uniformly
    from each of the dev card categories if n is randomly distributed. *)
let prob_choose_dev n dev_deck = 
  let lengths = length_dev_deck dev_deck in 
  if n < List.nth lengths 0 then Knight
  else if n < List.nth lengths 1 then Road
  else if n <  List.nth lengths 2 then Year
  else if n < List.nth lengths 3 then Monopoly
  else if n < List.nth lengths 4 then Victory
  else failwith "n cannot be more than the number of dev cards held"

(** [get_dev deck] randomly draws a dev card from the deck if it can be drawn,
    and returning Some of the deck, else None. *)
let get_dev dev_deck = 
  if dev_deck.num_dev = 0 then None
  else 
    let random_int = Random.int dev_deck.num_dev in 
    match prob_choose_dev random_int dev_deck with
    | Knight -> 
      Some {dev_deck with knight = dev_deck.knight - 1; num_dev = dev_deck.num_dev - 1}
    | Road -> 
      Some {dev_deck with road_building = dev_deck.road_building - 1; num_dev = dev_deck.num_dev - 1}
    | Year -> 
      Some {dev_deck with year_of_plenty = dev_deck.year_of_plenty - 1; num_dev = dev_deck.num_dev - 1}
    | Monopoly -> 
      Some {dev_deck with monopoly = dev_deck.monopoly - 1; num_dev = dev_deck.num_dev - 1}
    | Victory -> 
      Some {dev_deck with victory = dev_deck.victory - 1; num_dev = dev_deck.num_dev - 1}


let dev_of_string str = 
  if str = "knight" then Knight
  else if str = "road" then Road
  else if str = "year" then Year
  else if str = "monopoly" then Monopoly
  else if str = "victory" then Victory
  else failwith "Precondition violated : cannot give non dev"

(** [add_dev deck str] adds the [str] representing a dev card back
    to the pile. *)
let add_dev dev_deck dev_str = 
  let dev = dev_of_string dev_str in 
  match dev with
  | Knight -> 
    Some {dev_deck with knight = dev_deck.knight + 1; num_dev = dev_deck.num_dev + 1}
  | Road -> 
    Some {dev_deck with road_building = dev_deck.road_building + 1; num_dev = dev_deck.num_dev + 1}
  | Year -> 
    Some {dev_deck with year_of_plenty = dev_deck.year_of_plenty + 1; num_dev = dev_deck.num_dev + 1}
  | Monopoly -> 
    Some {dev_deck with monopoly = dev_deck.monopoly + 1; num_dev = dev_deck.num_dev + 1}
  | Victory -> 
    Some {dev_deck with victory = dev_deck.victory + 1; num_dev = dev_deck.num_dev + 1}









