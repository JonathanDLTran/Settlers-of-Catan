let c_NUM_CITIES = 4
let c_NUM_SETTLEMENTS = 5
let c_NUM_ROADS = 15

type player = {
  wheat : int;
  brick : int;
  ore : int;
  sheep : int;
  wood : int;
  num_army_cards : int;
  len_longest_road : int;
  victory_points : int;
  settlements_remaining : int;
  cities_remaining : int;
  roads_remaining : int;
  robber : bool;
}

let initialize_player () = {
  wheat = 0;
  brick = 0;
  ore = 0;
  sheep = 0;
  wood = 0 ;
  num_army_cards = 0;
  len_longest_road = 0;
  victory_points = 0;
  settlements_remaining = 
    robber = false;
}

let die_faces = 6

let roll () = 
  let () = Random.self_init () in 
  (Random.int (die_faces) + 1) + (Random.int (die_faces) + 1) 