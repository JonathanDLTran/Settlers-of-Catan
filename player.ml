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

  dev_played_already : bool;

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

  dev_played_already = false;

  len_longest_road = 0;

  longest_road = false;
  largest_army = false;

  victory_points = 0;

  settlements_remaining = c_NUM_SETTLEMENTS;
  cities_remaining = c_NUM_CITIES;
  roads_remaining = c_NUM_ROADS;

  robber = false;
}

let cheat_augment_resources player = {
  player with
  wheat = player.wheat + 100;
  brick = player.brick + 100;
  ore = player.ore + 100;
  wool = player.wool + 100;
  lumber = player.lumber + 100;
}

let add_dev_card_played_already player = {
  player with dev_played_already = true;
}

let reset_dev_played player = {
  player with dev_played_already = false;
}

(** [rool () is the dice roll of the player. ] *)
let roll () = 
  let () = Random.self_init () in 
  (Random.int (c_DIE_FACES) + 1) + (Random.int (c_DIE_FACES) + 1) 

(** [resources_to_list player] is a list of the values
    of the resources the player holds, wheat, brick, ore, wool, and lumber
    in that order. *)
let resources_to_tuple player = (
  player.wheat,
  player.brick,
  player.ore,
  player.wool,
  player.lumber
)

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
    bulk_add_resources (add_resource_n_times player res n) t 

(** [remove_resource_from_hand player resource] requires that removing [resource]
    form [player]'s hand will not make the hand have any negative values for resources. *)
let remove_resource_from_hand player resource =   
  match resource with
  | Ore -> {player with ore = player.ore - 1}
  | Wheat -> {player with wheat = player.wheat - 1}
  | Wool -> {player with wool = player.wool - 1}
  | Brick -> {player with brick = player.brick - 1}
  | Lumber -> {player with lumber = player.lumber - 1}

let rec remove_resource_n_times player resource n = 
  if n = 0 then player
  else
    remove_resource_n_times (remove_resource_from_hand player resource) resource (n - 1)

let rec bulk_remove_resources player res_list = 
  match res_list with 
  | [] -> player
  | (res, n) :: t ->
    bulk_remove_resources (remove_resource_n_times player res n) t

let sum_resources (a, b, c, d, e) = 
  a + b + c + d + e

let random_to_resource random_num = 
  match random_num with 
  | 0 -> Ore
  | 1 -> Wheat
  | 2 -> Wool
  | 3 -> Brick
  | 4 -> Lumber
  | _ -> failwith "Cannot be a random number greater than 4 or less than 0"

let greater_than_one_resources player = 
  (player.ore >= 1, player.wheat >= 1, player.wool >= 1, player.brick >= 1, player.lumber >= 1)

let non_zero_resources_list (ore, wheat, wool, brick, lumber) = 
  []
  |> (fun l -> if ore then 0 :: l else l)
  |> (fun l -> if wheat then 1 :: l else l)
  |> (fun l -> if wool then 2 :: l else l)
  |> (fun l -> if brick then 3 :: l else l)
  |> (fun l -> if lumber then 4 :: l else l)
  |> List.rev

let choose_non_zero non_zero_list = 
  if non_zero_list = [] then -1
  else begin
    let len = List.length non_zero_list in 
    let () = Random.self_init () in 
    let rand_pos = Random.int len in 
    List.nth non_zero_list rand_pos
  end

let remove_number player rand_pos = 
  match rand_pos with 
  | 0 -> remove_resource_from_hand player Ore
  | 1 -> remove_resource_from_hand player Wheat
  | 2 -> remove_resource_from_hand player Wool
  | 3 -> remove_resource_from_hand player Brick
  | 4 -> remove_resource_from_hand player Lumber
  | -1 -> player (* NO cards Do not remove anything *)
  | _ -> failwith "Cannot be a position greater than 4 or less than 0 as a random number, or not -1, which is no cards left"

let robber_choose_random_card player = 
  player 
  |> greater_than_one_resources
  |> non_zero_resources_list
  |> choose_non_zero
  |> (function 
        0 -> Some Ore | 
        1 -> Some Wheat | 
        2 -> Some Wool | 
        3 -> Some Brick | 
        4 -> Some Lumber | 
        -1 -> None |
        _ -> failwith "cannot not remove a card or not remove anything")

let rec randomly_remove_cards num_cards player = 
  if num_cards = 0 then player 
  else begin
    player 
    |> greater_than_one_resources
    |> non_zero_resources_list
    |> choose_non_zero
    |> remove_number player 
    |> randomly_remove_cards (num_cards - 1)
  end

let halve_hand player = 
  let resources = resources_to_tuple player in 
  let total = sum_resources resources in 
  if total > 7 
  then begin
    let num_removed = total / 2 in 
    randomly_remove_cards num_removed player
  end
  else player 

let check_tradeable_n n player resource = 
  match resource with
  | Ore -> player.ore >= n
  | Wheat -> player.wheat >= n
  | Wool -> player.wool >= n
  | Brick -> player.brick >= n
  | Lumber -> player.lumber >= n

let trade_resources_n n player res_start res_end = 
  if not (check_tradeable_n n player res_start) then player
  else 
    bulk_add_resources 
      (bulk_remove_resources player [(res_start, n)])  
      [(res_end, 1)]

let check_tradeable_4 player resource = 
  check_tradeable_n 4 player resource
(* match resource with
   | Ore -> player.ore >= 4
   | Wheat -> player.wheat >= 4
   | Wool -> player.wool >= 4
   | Brick -> player.brick >= 4
   | Lumber -> player.lumber >= 4 *)

let trade_resources_4 player res_start res_end = 
  trade_resources_n 4 player res_start res_end
(* if not (check_tradeable_4 player res_start) then player
   else 
   bulk_add_resources 
    (bulk_remove_resources player [(res_start, 4)])  
    [(res_end, 1)] *)

let check_tradeable_3 player resource = 
  check_tradeable_n 3 player resource

let trade_resources_3 player res_start res_end = 
  trade_resources_n 3 player res_start res_end

let check_tradeable_2 player resource = 
  check_tradeable_n 2 player resource

let trade_resources_2 player res_start res_end = 
  trade_resources_n 2 player res_start res_end


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

let dev_to_tuple player = (
  player.year_of_plenty,
  player.monopoly,
  player.victory_cards)

type dev = 
  | Year
  | Monopoly
  | Victory

let dev_cost player = 
  {player with 
   wheat = player.wheat - 1;
   wool = player.wool - 1;
   ore = player.ore - 1}

let add_dev dev player = 
  (match dev with
   | Year -> {player with year_of_plenty = player.year_of_plenty + 1}
   | Monopoly -> {player with monopoly = player.monopoly + 1}
   | Victory -> {player with victory_cards = player.victory_cards + 1})
  |> dev_cost

let play_dev dev player = 
  match dev with
  | Year -> {player with year_of_plenty = player.year_of_plenty - 1}
  | Monopoly -> {player with monopoly = player.monopoly - 1}
  | Victory -> 
    {player with 
     victory_cards = player.victory_cards - 1;
     victory_points = player.victory_points + 1;}

let has_resources_dev player = 
  player.ore >= 1 &&
  player.wool >= 1 &&
  player.wheat >= 1

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

let has_resources_road player = 
  player.lumber >= 1 &&
  player.brick >= 1

let can_build_settlement player = 
  player.settlements_remaining <> 0

let has_resources_settlement player = 
  player.lumber >= 1 &&
  player.brick >= 1 &&
  player.wool >= 1 &&
  player.wheat >= 1

let can_build_city player = 
  player.cities_remaining <> 0

let has_resources_city player = 
  player.wheat >= 2 &&
  player.ore >= 3

let pieces_to_list player = [
  player.settlements_remaining;
  player.cities_remaining;
  player.roads_remaining;
]

type piece = | City | Settlement | Road

let add_piece_pregame player piece = 
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
    {player with 
     roads_remaining = player.roads_remaining - 1}

let add_piece player piece = 
  match piece with
  | City -> 
    {player with 
     wheat = player.wheat - 2;
     ore = player.ore - 3;
     cities_remaining = player.cities_remaining - 1;
     settlements_remaining = player.settlements_remaining + 1;
     victory_points = player.victory_points + 1;} (* as cities can only be build on settlementds *)
  | Settlement -> 
    {player with 
     lumber = player.lumber - 1;
     brick = player.brick - 1;
     wool = player.wool - 1;
     wheat = player.wheat - 1;
     settlements_remaining = player.settlements_remaining - 1;
     victory_points = player.victory_points + 1;
    }
  | Road -> 
    {player with 
     lumber = player.lumber - 1;
     brick = player.brick - 1;
     roads_remaining = player.roads_remaining - 1}

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

let print_string_of_player_info (p : player) = 
  let robber_status = get_robber p in 
  let cities = p.cities_remaining in 
  let settlements = p.settlements_remaining in 
  let roads = p.roads_remaining in 
  let knights_held = p.knights_held in 
  let knights_played = p.knights_played in 
  let most_knights = is_largest_army p in 
  let (y, m, vc) = dev_to_tuple p in 
  let road_length = get_length_road p in 
  let most_road = is_longest_road p in 
  let (w, b, o, wl, l) = resources_to_tuple p in 
  let victory_points = get_victory_points p in 
  ANSITerminal.(print_string [black] "\n");
  ANSITerminal.(print_string [red] ("\nRobber control status : " ^ string_of_bool robber_status ));
  ANSITerminal.(print_string [black] "\n");
  ANSITerminal.(print_string [cyan] ("\nNumber of cities remaining : " ^ string_of_int cities ));
  ANSITerminal.(print_string [cyan] ("\nNumber of settlements remaining : " ^ string_of_int settlements ));
  ANSITerminal.(print_string [cyan] ("\nNumber of roads remaining : " ^ string_of_int roads ));
  ANSITerminal.(print_string [black] "\n");
  ANSITerminal.(print_string [blue] ("\nNumber of knights in hand : " ^ string_of_int knights_held ));
  ANSITerminal.(print_string [blue] ("\nNumber of knights played : " ^ string_of_int knights_played ));
  ANSITerminal.(print_string [blue] ("\nLargest army? : " ^ string_of_bool most_knights ));
  ANSITerminal.(print_string [black] "\n");
  ANSITerminal.(print_string [yellow] ("\nNumber of year of plenty : " ^ string_of_int y ));
  ANSITerminal.(print_string [yellow] ("\nNumber of monopoly : " ^ string_of_int m ));
  ANSITerminal.(print_string [yellow] ("\nNumber of Victory Point Cards : " ^ string_of_int vc ));
  ANSITerminal.(print_string [black] "\n");
  ANSITerminal.(print_string [green] ("\nLongest Road length : " ^ string_of_int road_length ));
  ANSITerminal.(print_string [green] ("\nLongest Road? : " ^ string_of_bool most_road ));
  ANSITerminal.(print_string [black] "\n");
  ANSITerminal.(print_string [magenta] ("\nNumber of wheat : " ^ string_of_int w ));
  ANSITerminal.(print_string [magenta] ("\nNumber of brick : " ^ string_of_int b ));
  ANSITerminal.(print_string [magenta] ("\nNumber of ore : " ^ string_of_int o ));
  ANSITerminal.(print_string [magenta] ("\nNumber of wool : " ^ string_of_int wl ));
  ANSITerminal.(print_string [magenta] ("\nNumber of lumber : " ^ string_of_int l ));
  ANSITerminal.(print_string [black] "\n");
  ANSITerminal.(print_string [blue; Bold;] ("\nNumber of Victory Points : " ^ string_of_int victory_points ));
  ANSITerminal.(print_string [black] "\n");
  ANSITerminal.(print_string [black] "\n")



