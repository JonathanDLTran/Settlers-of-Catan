let c_NUM_CITIES = 4
let c_NUM_SETTLEMENTS = 5
let c_NUM_ROADS = 15
let c_DIE_FACES = 6
let c_VICTORY_REQ = 10

type player = {
  wheat : int;
  brick : int;
  ore : int;
  wool : int;
  lumber : int;

  knights_held : int;
  knights_played : int;

  year_of_plenty : int;
  monopoly : int;
  victory_cards : int;

  len_longest_road : int;

  longest_road : bool;
  largest_army : bool;

  victory_points : int;

  settlements_remaining : int;
  cities_remaining : int;
  roads_remaining : int;

  robber : bool;
}

type t = player

let initialize_player = {
  wheat = 0;
  brick = 0;
  ore = 0;
  wool = 0;
  lumber = 0 ;

  knights_held = 0 ;
  knights_played = 0;

  year_of_plenty = 0;
  monopoly = 0;
  victory_cards = 0;

  len_longest_road = 0;

  longest_road = false;
  largest_army = false;

  victory_points = 0;

  settlements_remaining = c_NUM_SETTLEMENTS;
  cities_remaining = c_NUM_CITIES;
  roads_remaining = c_NUM_ROADS;

  robber = false;
}

(** [rool () is the dice roll of the player. ] *)
let roll () = 
  let () = Random.self_init () in 
  (Random.int (c_DIE_FACES) + 1) + (Random.int (c_DIE_FACES) + 1) 

(** [resources_to_list player] is a list of the values
    of the resources the player holds, wheat, brick, ore, wool, and lumber
    in that order. *)
let resources_to_list player = [
  player.wheat;
  player.brick;
  player.ore;
  player.wool;
  player.lumber;
]

type resource = | Ore | Wheat | Wool | Brick | Lumber

let add_resource_to_hand player resource = 
  match resource with
  | Ore -> {player with ore = player.ore + 1}
  | Wheat -> {player with wheat = player.wheat + 1}
  | Wool -> {player with wool = player.wool + 1}
  | Brick -> {player with brick = player.brick + 1}
  | Lumber -> {player with lumber = player.lumber + 1}

let rec add_resource_n_times player resource n  = 
  if n = 0 then player
  else 
    add_resource_n_times (add_resource_to_hand player resource) resource (n - 1) 

let rec bulk_add_resources player res_list = 
  match res_list with
  | [] -> player
  | (res, n) :: t ->
    bulk_add_resources (add_resource_n_times player res n ) t 

let remove_resource_from_hand player resource =   
  match resource with
  | Ore -> {player with ore = player.ore - 1}
  | Wheat -> {player with wheat = player.wheat - 1}
  | Wool -> {player with wool = player.wool - 1}
  | Brick -> {player with brick = player.brick - 1}
  | Lumber -> {player with lumber = player.lumber - 1}

let rec bulk_remove_resources player res_list = 
  failwith "Unimplemented"

let check_tradeable player resource = 
  match resource with
  | Ore -> player.ore >= 4
  | Wheat -> player.wheat >= 4
  | Wool -> player.wool >= 4
  | Brick -> player.brick >= 4
  | Lumber -> player.lumber >= 4

let trade_resources player res_start res_end = 
  if not (check_tradeable player res_start) then player
  else 
    bulk_add_resources 
      (bulk_remove_resources player [(res_start, 4)])  
      [(res_end, 1)]

let knights_to_list player = [
  player.knights_held;
  player.knights_played;
]

let add_knight player = {
  player with knights_held = player.knights_held + 1
}

let play_knight player = {
  player with 
  knights_held = player.knights_held - 1;
  knights_played = player.knights_played + 1;
}

let dev_to_list player = [
  player.year_of_plenty;
  player.monopoly;
  player.victory_cards;
]

type dev = 
  | Year
  | Monopoly
  | Victory

let add_dev dev player = 
  match dev with
  | Year -> {player with year_of_plenty = player.year_of_plenty + 1}
  | Monopoly -> {player with monopoly = player.monopoly + 1}
  | Victory -> {player with victory_cards = player.victory_cards + 1}

let play_dev dev player = 
  match dev with
  | Year -> {player with year_of_plenty = player.year_of_plenty - 1}
  | Monopoly -> {player with monopoly = player.monopoly - 1}
  | Victory -> 
    {player with 
     victory_cards = player.victory_cards - 1;
     victory_points = player.victory_points + 1;}

let is_longest_road player = 
  player.longest_road

let is_largest_army player = 
  player.largest_army

let set_longest_road player status = 
  if status then 
    {player with 
     longest_road = true;
     victory_points = player.victory_points + 2;}
  else 
    {player with 
     longest_road = false;}

let set_largest_army player status = 
  if status then 
    {player with 
     largest_army = true;
     victory_points = player.victory_points + 2;}
  else 
    {player with 
     largest_army = false;}

let get_length_road player = player.len_longest_road

let set_length_road player len = {
  player with len_longest_road = len
}

let get_victory_points player = player.victory_points

let set_victory_points player points = {
  player with victory_points = points
}

let check_victory player = 
  player.victory_points = c_VICTORY_REQ

let can_build_road player = 
  player.roads_remaining <> 0

let can_build_settlement player = 
  player.settlements_remaining <> 0

let can_build_city player = 
  player.cities_remaining <> 0

let pieces_to_list player = [
  player.settlements_remaining;
  player.cities_remaining;
  player.roads_remaining;
]

type piece = | City | Settlement | Road

let add_piece player piece = 
  match piece with
  | City -> 
    {player with 
     cities_remaining = player.cities_remaining - 1;
     victory_points = player.victory_points + 2;}
  | Settlement -> 
    {player with 
     settlements_remaining = player.settlements_remaining - 1;
     victory_points = player.victory_points + 1;
    }
  | Road -> 
    {player with roads_remaining = player.roads_remaining - 1}

let remove_piece player piece = 
  match piece with
  | City -> 
    {player with 
     cities_remaining = player.cities_remaining + 1;
     victory_points = player.victory_points - 2;}
  | Settlement -> 
    {player with 
     settlements_remaining = player.settlements_remaining + 1;
     victory_points = player.victory_points - 1;}
  | Road -> 
    {player with roads_remaining = player.roads_remaining + 1}

let get_robber player = player.robber

let set_robber player status = {
  player with robber = status
}



