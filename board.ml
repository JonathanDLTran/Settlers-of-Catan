(* ######## TYPES related to board ######## *)

type tile = 
  | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O
  | P | Q | R | S 

type resource = 
  | Wheat
  | Ore 
  | Wool 
  | Lumber 
  | Brick
  | Desert

(* ######## RESOURCE CONVERSION ######### *)

(** [string_to_resource s] is the resource corresponding
    to [s]. *)
let string_to_resource s = 
  if s = "wheat" then Wheat 
  else if s = "ore" then Ore
  else if s = "wool" then Wool 
  else if s = "lumber" then Lumber
  else if s = "brick" then Brick
  else failwith "Not a valid resource"

(* ####### BOARD INSTANTIATION ########## *)

let three_to_one_port_nodes = [
  1; 2; 37; 43; 49; 53; 42; 48
]

let two_to_one_port_nodes = [
  (11, Wool); (12, Wool); 
  (24, Ore); (30, Ore); 
  (51, Wheat); (52, Wheat); 
  (19, Brick); (25, Brick); 
  (3, Lumber); (8, Lumber);
]

(** [tile_to_node tile] is the list of nodes that are on the boundary
    of [tile]. *)
let tile_to_node (tile : tile) : int list = 
  match tile with 
  | A -> [1; 2; 4; 5; 9; 10]
  | B -> [3; 4; 8; 9; 14; 15]
  | C -> [5; 6; 10; 11; 16; 17]
  | D -> [7; 8; 13; 14; 19; 20]
  | E -> [9; 10; 15; 16; 21; 22]
  | F -> [11; 12; 17; 18; 23; 24]
  | G -> [14; 15; 20; 21; 26; 27]
  | H -> [16; 17; 22; 23; 28; 29]
  | I -> [19; 20; 25; 26; 31; 32]
  | J -> [21; 22; 27; 28; 33; 34]
  | K -> [23; 24; 29; 30; 35; 36]
  | L -> [26; 27; 32; 33; 38; 39]
  | M -> [28; 29; 34; 35; 40; 41]
  | N -> [31; 32; 37; 38; 43; 44]
  | O -> [33; 34; 39; 40; 45; 46]
  | P -> [35; 36; 41; 42; 47; 48]
  | Q -> [38; 39; 44; 45; 49; 50]
  | R -> [40; 41; 46; 47; 51; 52]
  | S -> [45; 46; 50; 51; 53; 54] 

let robber_start_tile = J

let tile_resource_value = [
  (A, Wheat, 12);
  (B, Lumber, 8);
  (C, Wheat, 9);
  (D, Brick, 5);
  (E, Lumber, 11);
  (F, Ore, 10);
  (G, Ore, 3);
  (H, Brick, 6);
  (I, Wheat, 6);
  (J, Desert, 7);
  (K, Wool, 2);
  (L, Wheat, 4);
  (M, Wool, 4);
  (N, Wool, 11);
  (O, Lumber, 3);
  (P, Lumber, 9);
  (Q, Wool, 5);
  (R, Brick, 10);
  (S, Ore, 8);
]

let c_NUM_NODES = 54

(* ######## TILE CONVERSION ######### *)

(** [tile_to_string tile] is the corresponding string of the tile.  *)
let tile_to_string tile = 
  match tile with 
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"
  | G -> "G"
  | H -> "H"
  | I -> "I"
  | J -> "J"
  | K -> "K"
  | L -> "L"
  | M -> "M"
  | N -> "N"
  | O -> "O"
  | P -> "P"
  | Q -> "Q"
  | R -> "R"
  | S -> "S"

(** [string_to_tile s] is the corresponding tile of the [s].  *)
let string_to_tile s = 
  match s with 
  | "A" -> A
  | "B" -> B
  | "C" -> C
  | "D" -> D
  | "E" -> E
  | "F" -> F
  | "G" -> G
  | "H" -> H
  | "I" -> I
  | "J" -> J
  | "K" -> K
  | "L" -> L
  | "M" -> M
  | "N" -> N
  | "O" -> O
  | "P" -> P
  | "Q" -> Q
  | "R" -> R
  | "S" -> S
  | _ -> failwith "Not a valid string for a tile"

(* ########NODE NEIGHBOR HELPERS ######## *)

let special_nodes = [
  1; 2; 3; 4; 5; 6; 
  7; 8; 9; 10; 11; 12; 
  13; 18; 25; 30; 37; 42;
  43; 44; 45; 46; 47; 48; 
  49; 50; 51; 52; 53; 54
]

(** [special_neighbors node] is all the neighbors of [nodes] with
    only 2 neighbors, namely those on the edge of the board. *)
let special_neighbors (node : int) : int list = 
  if not (List.mem node special_nodes) then failwith "Must be special node"
  else begin 
    match node with
    | 1 -> [2; 4]
    | 2 -> [1; 5]
    | 3 -> [4; 8]
    | 4 -> [1; 3; 9]
    | 5 -> [2; 6; 10]
    | 6 -> [5; 11]
    | 7 -> [8; 13]
    | 8 -> [3; 7; 14]
    | 9 -> [4; 10; 15]
    | 10 -> [5; 9; 16]
    | 11 -> [6; 12; 17]
    | 12 -> [11; 18]
    | 13 -> [7; 19]
    | 18 -> [12; 24]
    | 25 -> [19; 31]
    | 30 -> [24; 36]
    | 37 -> [31; 43]
    | 42 -> [36; 48]
    | 43 -> [37; 44]
    | 44 -> [38; 43; 49]
    | 45 -> [39; 46; 50]
    | 46 -> [40; 45; 51]
    | 47 -> [41; 48; 52]
    | 48 -> [42; 47]
    | 49 -> [44; 50]
    | 50 -> [45; 49; 53]
    | 51 -> [46; 52; 54]
    | 52 -> [51; 47]
    | 53 -> [50; 54]
    | 54 -> [53; 51]
    | _ -> failwith "Must be special node"
  end

(** [direction_chooser n1] is [true] iff when [n1] is even, 
    [n1 - 1] is a neighbor on a horizontal of [n1]. *)
let direction_chooser n1 = 
  if n1 < 13 || (n1 >= 19 && n1 < 25) || (n1 >= 31 && n1 < 37) || n1 >= 43 then begin
    n1 mod 2 = 0 
  end
  else n1 mod 2 <> 0

(** [neighbors node] is the neighbors of [node] on the board. 

    Requires: [node] must have three neighbors on the baord. It cannot
    be in [special_nodes]. *)
let neighbors (node : int) : int list = 
  assert (not (List.mem node special_nodes));
  if direction_chooser node then [(node - 6); (node - 1); (node + 6)]
  else [(node - 6); (node + 1); (node + 6)]

(** [node_neighbors node] is all the neighbors of [node]. *)
let node_neighbors (node : int) : int list = 
  assert (node >= 1 && node <= c_NUM_NODES);
  if List.mem node special_nodes then special_neighbors node
  else neighbors node 

(* ############ BOARD OVERVIEW ########### *)

type player = bool

type structure = 
  | Settlement
  | City

type board = {
  tile_info : (tile * resource * int) list;
  robber_node : tile;
  edges_occupied : (int * int * player) list;
  nodes_occupied : (int * player * structure) list;
}

type t = board

let instantiate_board = {
  tile_info = tile_resource_value;
  robber_node = robber_start_tile;
  edges_occupied = [];
  nodes_occupied = [];
}

type error = 
  | PostionOccupiedErr
  | AdjacentPositionErr 
  | UnconnectedErr
  | SettlmentMissingErr
  | NotAnEdgeErr

type action = 
  | Success of board
  | Failure of error * board

(* ####### ROBBER ######## *)

(** [get_robber_tile board] is the string corresponding to the tile
    the robber is on in [board]. *)
let get_robber_tile board = 
  board.robber_node |> tile_to_string

(** [get_robber_tile c board] is the [board] with the robber at 
    the tile correspionding to [c].

    Requires [c] is a char between 'A' and 'S' inclusive. *)
let set_robber_tile c board = 
  assert (c >= 'A' && c <= 'S');
  {board with robber_node = c |> Char.escaped |> string_to_tile}

(** [player_tile_intersect player board] is [true] iff
    [player] has a node on the [tile] in [board]. *)
let player_tile_intersect tile player board = 
  let tile_nodes = tile_to_node tile in 
  let player_nodes = 
    board.nodes_occupied
    |> List.filter (fun (n, p, s) -> p = player)
    |> List.map (fun (n, p, s) -> n) in 
  List.exists (fun player_node -> List.mem player_node tile_nodes) player_nodes

(** [can_robber_steal_card  player board] is [true] iff
    [player] has a node on the tile of the robber in [board]. *)
let can_robber_steal_card player board = 
  let tile = board.robber_node in 
  player_tile_intersect tile player board

(* ##### Settlements ######### *)

(** [check_neighbors node node_list] is [true] iff [node]'s neighbors 
    are not in [node_list], that is to say [node] does not 
    neighbor any node in [nodes_list]*)
let check_neighbors (node : int) (node_list : (int * player * structure) list) : bool = 
  let reduced_node_list = List.map (fun (n, _, _) -> n) node_list in 
  node 
  |> node_neighbors
  |> List.for_all (fun neighbor -> not (List.mem neighbor reduced_node_list))

(** [check_node_occupied node node_list] is [true] iff [node] 
    is occupied in [node_list]. *)
let check_node_occupied node node_list = 
  let reduced_nodes = List.map (fun (n, _ , _) -> n) node_list in 
  List.mem node reduced_nodes 

(** [node_connected_to_road node player road_list] is [true] iff
    [node] is connected to a road in [road_list] for [player]. *)
let rec node_connected_to_road node player road_list =
  match road_list with
  | [] -> false 
  | (n1, n2, p) :: t ->
    if (n1 = node || n2 = node) && p = player then true 
    else node_connected_to_road node player t

(** [add_settlement node player board] adds a settlement to the board
    given that the settlement is legal and is connected to another road. *)
let add_settlement node player board = 
  (* check node not occupied in first place *)
  if check_node_occupied node board.nodes_occupied 
  then (Failure (PostionOccupiedErr, board))
  (* check node is not directly adjacent to other nodes *)
  else if not (check_neighbors node board.nodes_occupied) 
  then (Failure (AdjacentPositionErr, board))
  (* check node connects to a prexistint road for the player *)
  else if not (node_connected_to_road node player board.edges_occupied) 
  then (Failure (UnconnectedErr, board))
  (* add the settlement *)
  else Success ({board with nodes_occupied = (node, player, Settlement) :: board.nodes_occupied })

(** [add_settlement_pregame node player board] adds a settlement
    in the pregame. Does not require that the settlement is connect
    to another road. *)
let add_settlement_pregame node player board = 
  (* check node not occupied in first place *)
  if check_node_occupied node board.nodes_occupied 
  then (Failure (PostionOccupiedErr, board))
  (* check node is not directly adjacent to other nodes *)
  else if not (check_neighbors node board.nodes_occupied) 
  then (Failure (AdjacentPositionErr, board))
  (* add the settlement *)
  else Success ({board with nodes_occupied = (node, player, Settlement) :: board.nodes_occupied })

(** [check_node_is_settlement node player node_list] checks that [node]
    is a [Settlement] for [player]. *)
let rec check_node_is_settlement node player node_list = 
  match node_list with 
  | [] -> false 
  | (n, p, structure) :: t->
    if n = node && player = p then structure = Settlement 
    else check_node_is_settlement node player t

(* ##### Cities ######### *)

(** [add_city node player board] adds a city to board
    given it is a legal position to add the node.  *)
let add_city node player board = 
  if not (check_node_is_settlement node player board.nodes_occupied)
  then (Failure (SettlmentMissingErr, board))
  (* add the city *)
  else 
    let old_nodes = board.nodes_occupied in 
    let intermediate_nodes = List.filter (fun (n, _, _) -> n <> node) old_nodes in 
    Success ({board with nodes_occupied = 
                           (node, player, City) :: intermediate_nodes })

(* 
  (* check node not occupied in first place *)
  if check_node_occupied node board.nodes_occupied 
  then begin 
    if not (check_node_is_settlement node player board.nodes_occupied)
    then (Failure (SettlmentMissingErr, board))
    (* add the city *)
    else Success ({board with nodes_occupied = (node, player, Settlement) :: board.nodes_occupied })
  end
  (* check node is not directly adjacent to other nodes *)
  else if not (check_neighbors node board.nodes_occupied) 
  then (Failure (AdjacentPositionErr, board))
  (* check node connects to a prexistint road for the player *)
  else if not (node_connected_to_road node player board.edges_occupied) 
  then (Failure (UnconnectedErr, board))
  (* add the city *)
  else Success ({board with nodes_occupied = (node, player, Settlement) :: board.nodes_occupied }) 
  *)

(* #########resource collection ######## *)

let tiles = [
  A;B;C;D;E;F;G;H;I;J;K;L;M;N;O;P;Q;R;S
]

(** [char_to_tile c] is the tile representing [c].  *) 
let char_to_tile c = 
  assert (c >= 'A' && c <= 'Z');
  List.nth tiles (Char.code c - Char.code 'A') 

(** [rool_to_tile value] is the list of tiles that 
    has a node vertex at [value].  *)
let roll_to_tile value = 
  assert (value >= 2 && value <= 12);
  List.filter (fun (_, _, v) -> v = value) tile_resource_value

(** [tile_to_neighbors tile_list] is the list of 
    neighbors for the tiles in [tile_list] tagged with 
    the resource the neighbor had. *)
let rec tile_to_neighbors tile_list = 
  match tile_list with 
  | [] -> []
  | (tile, resource, value) :: t ->
    tile
    |> tile_to_node  
    |> List.map (fun n -> (n, resource))
    |> (@) (tile_to_neighbors t)

type resource_hoard = {
  wheat : int;
  ore : int ;
  wool : int;
  brick : int;
  lumber : int;
}

let empty_resource_hoard = {
  wheat = 0;
  ore = 0;
  wool = 0;
  brick = 0;
  lumber = 0;
}

(** [update_resource_hoard resource num_cards hoard] updates
    the [hoard] with the current amount of [resource]
    based on [num_cards]. *)
let update_resource_hoard resource num_cards hoard = 
  match resource with 
  | Wheat -> 
    {hoard with wheat = num_cards + hoard.wheat}
  | Ore -> 
    {hoard with ore = num_cards + hoard.ore}
  | Wool -> 
    {hoard with wool = num_cards + hoard.wool}
  | Lumber -> 
    {hoard with lumber = num_cards + hoard.lumber}
  | Brick -> 
    {hoard with brick = num_cards + hoard.brick}
  | Desert -> hoard

(** [structure_to_num_cards structure] converts a structure
    to the equivalent number of card resources that structure produces.  *)
let structure_to_num_cards structure = 
  match structure with 
  | Settlement -> 1
  | City -> 2

(** [mem_nodes_occupied node player nodes_occupied robber_nodes] is the number
    of cards a player would get given [node] for [player], 
    given that [node] is not blocked by a robber by [robber_nodes].  *)
let rec mem_nodes_occupied node player nodes_occupied robber_nodes = 
  match nodes_occupied with 
  | [] -> 0
  | (n, p, structure) :: t ->
    if n = node && p = player && (not (List.mem n robber_nodes)) 
    then structure_to_num_cards structure 
    else mem_nodes_occupied node player t robber_nodes

(** [neighbors_list_to_resources player resource_hoard board neighbors] 
    is the [resource hoard] for the [player] based on [neighbors] 
    and the game [board]. *)
let rec neighbors_list_to_resources player resource_hoard board robber_nodes neighbors = 
  match neighbors with 
  | [] -> resource_hoard
  | (node, resource) :: t ->  
    let num_cards = mem_nodes_occupied node player board.nodes_occupied robber_nodes in 
    let new_hoard = update_resource_hoard resource num_cards resource_hoard in 
    neighbors_list_to_resources player new_hoard board robber_nodes t 

(** [robber_to_neighbors board] is the list of 
    nodes that are bounded around the robber tile for the current
    [board].  *)
let robber_to_neighbors board = 
  let robber_tile = board.robber_node in 
  tile_to_node robber_tile

(** [add_resources roll board] is the pair of resource hoards
    for the resources that player1 and player2 would pick up
    upon the [roll] on the game [board] at the current state.  *)
let get_resources roll board = 
  let robber_nodes = robber_to_neighbors board in 
  roll 
  |> roll_to_tile 
  |> tile_to_neighbors
  |> (fun neighbors ->
      (neighbors_list_to_resources
         true empty_resource_hoard board robber_nodes neighbors,
       neighbors_list_to_resources 
         false empty_resource_hoard board robber_nodes neighbors))

(* ############### EDGE STUFF ########### *)

let print_triples_list edges = 
  List.map (fun (n1, n2, p) -> 
      "(" ^ string_of_int n1 ^ " , " ^ string_of_int n2 ^ " , " ^ string_of_bool p ^ ")" 
      |> print_endline) edges
  |> ignore

(** [connected node1 node2 player edges] 
    is [true] iff either [node1] or [node2] connects
    to the rest of [edges]. *)
let connected node1 node2 player edges = 
  if List.filter (fun (n1, n2, p) -> (n1 = node1 || n2 = node2 || n1 = node2 || n2 = node1) && player = p) edges <> [] then true 
  else false

(** [check_nodes_form_edge node1 node2] is [true] iff [node1] and
    [node2] form a legal edge in the game. *)
let check_nodes_form_edge node1 node2 = 
  let node1_neighbors = node_neighbors node1 in 
  List.mem node2 node1_neighbors

(** [check_edge_connected_to_settlement player node1 node2 board] is [true]
    iff either [node1] or [node2] is connected to a [Settlement] thatn [player]
    has already placed on [board]. *)
let rec check_edge_connected_to_settlement player node1 node2 nodes = 
  match nodes with 
  | [] -> false 
  | (n, p, structure) :: t ->
    if (n = node1 || n = node2) && p = player && structure = Settlement then true 
    else check_edge_connected_to_settlement player node1 node2 t 

(** [check_edge node1 node2 player edges] is [true] iff
    [node1] and [node2] form a valid edge for the [board],
    [node1] is less than [node2], 
    [node1] and [node2] specify an unbuilt edge prebiously in [edges]  *)
let check_edge node1 node2 player edges = 
  if (node1 <= 1 && node1 >= c_NUM_NODES) then false 
  else if (node2 <= 1 && node2 >= c_NUM_NODES) then false 
  else if node1 >= node2 then false
  else if node1 = node2 then false 
  else if not (check_nodes_form_edge node1 node2) then false
  else if List.filter (fun (n1, n2, _) -> (n1 = node1 && n2 = node2)) edges <> [] then false (* node not occupied *)
  else true

(** [add_road node1 node2 player board] iadds an edge [node1], [node2]
    to the [board] for [player] given a valid edge [node1] [node2] 

    Requires that the road to add connects to a previous road on the board. *)
let add_road node1 node2 player board = 
  if (node1 <= 1 && node1 >= c_NUM_NODES) then (Failure (NotAnEdgeErr, board))
  else if (node2 <= 1 && node2 >= c_NUM_NODES) then (Failure (NotAnEdgeErr, board))
  else if node1 >= node2 then (Failure (NotAnEdgeErr, board))
  else if node1 = node2 then (Failure (NotAnEdgeErr, board))
  else if not (check_nodes_form_edge node1 node2) then (Failure (NotAnEdgeErr, board))
  else if List.filter (fun (n1, n2, _) -> (n1 = node1 && n2 = node2)) board.edges_occupied <> [] then (Failure (PostionOccupiedErr, board)) (* node not occupied *) 
  else if not (connected node1 node2 player board.edges_occupied) then (Failure (UnconnectedErr, board)) (* node connected to another node *)
  else Success {board with edges_occupied = (node1, node2, player) :: board.edges_occupied}

(** [add_road_pregame node1 node2 player board] iadds an edge [node1], [node2]
    to the [board] for [player] given a valid edge [node1] [node2] 

    Does not require that the edge connects to anither edge, only requires than an
    edge connects to an existing settlement. 

    Requires: To be used only after a settlement is placed in the pregame for the player. *)
let add_road_pregame node1 node2 player board = 
  if (node1 <= 1 && node1 >= c_NUM_NODES) then (Failure (NotAnEdgeErr, board))
  else if (node2 <= 1 && node2 >= c_NUM_NODES) then (Failure (NotAnEdgeErr, board))
  else if node1 >= node2 then (Failure (NotAnEdgeErr, board))
  else if node1 = node2 then (Failure (NotAnEdgeErr, board))
  else if not (check_nodes_form_edge node1 node2) then (Failure (NotAnEdgeErr, board))
  else if List.filter (fun (n1, n2, _) -> (n1 = node1 && n2 = node2)) board.edges_occupied <> [] then (Failure (PostionOccupiedErr, board)) (* node not occupied *) 
  else if not (check_edge_connected_to_settlement player node1 node2 board.nodes_occupied) then (Failure (UnconnectedErr, board)) (* node connected to another node *)
  else Success {board with edges_occupied = (node1, node2, player) :: board.edges_occupied}

(* ######### Road Length algorithm ######## *)

(** [get_player_edges player edge_list acc] are the edges from [edge_list]
    that [player] controls on the board.  *)
let rec get_player_edges player edge_list acc = 
  match edge_list with 
  | [] -> acc 
  | (n1, n2, p) :: t ->  
    if player = p then get_player_edges player t ((n1, n2) :: acc)
    else get_player_edges player t acc

(** [player_unique_nodes edges] are the unique nodes in [edges]
    sorted in ascending order.  *)
let player_unique_nodes edges = 
  edges
  |> List.split
  |> (fun (lst1, lst2) -> lst1 @ lst2)
  |> List.sort_uniq compare

(** [check_visited node edges] is [true] iff
    [node] in [nodes_visited] and thus has been visited.
    Requires: [node] is a node connected in [edges]. *)
let check_visited node nodes_visited = 
  List.mem node nodes_visited

(** [get_neighbors node edges acc] are the neighbors of
    [node] in [edges].
    Requires: [node] is a node connected in [edges]. *)
let rec get_neighbors node edges acc = 
  match edges with 
  | [] -> acc
  | (n1, n2) :: t ->
    if n1 = node then get_neighbors node t (n2 :: acc)
    else if n2 = node then get_neighbors node t (n1 :: acc)
    else get_neighbors node t (acc)

let my_max lst = 
  match lst with 
  | [] -> failwith "lst cannot be empty"
  | h :: [] -> h 
  | h :: t as lst ->
    lst 
    |> List.sort compare 
    |> List.rev
    |> (fun l -> List.nth l 0 + List.nth l 1)

let all_nodes_visited neighbors nodes_visited = 
  List.exists (fun node -> List.mem node nodes_visited) neighbors

let rec get_unvisited_nodes neighbors nodes_visited acc = 
  match neighbors with 
  | [] -> acc
  | h :: t ->
    if List.mem h nodes_visited then get_unvisited_nodes t nodes_visited acc 
    else get_unvisited_nodes t nodes_visited (h :: acc) 

(* let rec visit_neighbor node edges nodes_visited hash_table =
   if not (check_visited node nodes_visited) then fixed_node_longest node edges nodes_visited hash_table
   else 
    (* let length, nodes_visited' = fixed_node_longest node edges nodes_visited hash_table in *)
    failwith "Unimplemented" *)

let top_two_max x y z = 
  [x; y; z]
  |> List.sort compare 
  |> List.rev 
  |> (fun l -> 
      match l with 
      | x :: y :: z :: [] -> x + y 
      | _ -> failwith "Precondition: all lists have three elements")

(** [fixed_node_longest node edges nodes_visited hash_table] is the length of the longest
    path that passes through [node] in [edges].updates [nodes_visited] every time 
    an unvisited node is visited for the first time.
    Requires: [node] is unvisited before visiting.
    Updates hash table each time the three longest paths
    enter a node is known, and if possible also uses
    memoized information in the [hash_table] to determine
    longest path
    Imperative with Hashtable. *)
let rec fixed_node_longest node edges nodes_visited hash_table = 
  let neighbors = get_neighbors node edges [] in 
  let nodes_visited' = node :: nodes_visited in 
  if List.length neighbors = 1 && all_nodes_visited neighbors nodes_visited' then (0, nodes_visited') (* ending tail case *)
  else if all_nodes_visited neighbors nodes_visited' then (1, nodes_visited') (* circular case *)
  else begin 
    let unvisited = get_unvisited_nodes neighbors nodes_visited' [] in 
    match unvisited with 
    | [] -> failwith "Must have at least one neighbor"
    | h :: [] -> 
      let length1, nodes_visited1 = fixed_node_longest h edges nodes_visited' hash_table in 
      (1 + length1, nodes_visited1)
    | h1 :: h2 :: [] -> 
      let length1, nodes_visited1 = fixed_node_longest h1 edges nodes_visited' hash_table in 
      let length2, nodes_visited2 = fixed_node_longest h2 edges nodes_visited1 hash_table in 
      (1 + length1 + 1 + length2, nodes_visited2)
    | h1 :: h2 :: h3 :: [] ->
      let length1, nodes_visited1 = fixed_node_longest h1 edges nodes_visited' hash_table in 
      let length2, nodes_visited2 = fixed_node_longest h2 edges nodes_visited1 hash_table in
      let length3, nodes_visited3 = fixed_node_longest h3 edges nodes_visited2 hash_table in  
      length1 |> string_of_int |> print_endline;
      length2 |> string_of_int |> print_endline;
      length3 |> string_of_int |> print_endline;
      (1 + 1 + top_two_max length1 length2 length3, nodes_visited3)
    | h :: t -> failwith "Cannot have more than three neighbors"
  end
(* | [] -> 
   | h :: t as list ->
   List.fold_left (fun (x, y) n -> ) (nodes_visited', []) list

    if not (check_visited h nodes_visited') then 1 + fixed_node_longest h edges nodes_visited' hash_table *)


(* if Hashtbl.mem hash_table node then Hashtbl.find hash_table node |> my_max 
   else begin 
   (* add node to nodes_visited *)
   let nodes_visited' = node :: nodes_visited in 
   let neighbors = get_neighbors node edges [] in 
   (* No neighbors -> return as finished with length 0 (no paths emanating from this node) *)
   match neighbors with 
   | [] -> failwith "node must have at least one neighbor on the map"
   | h :: t as lst -> 
    lst 
    |> List.map (fun n -> if not (check_visited n nodes_visited') then let length = fixed_node_longest n edges nodes_visited' hash_table in Hashtbl.add hash_table n ((1 + length) :: Hashtbl.find hash_table node); 1 + length else let () = Hashtbl.add hash_table n (1 :: Hashtbl.find hash_table node) in 1) 
    |> my_max
   end *)

let get_max start lst = 
  List.fold_left (fun init n -> if n > init then n else init) start lst

(** [longest_road board] is the length of the longest road
    on [board] for [player].  Imperative with Hashtable. *)
let longest_road player board = 
  let player_edges = get_player_edges player board.edges_occupied [] in 
  let unique_nodes = player_unique_nodes player_edges in 
  let hash_table = Hashtbl.create (List.length unique_nodes) in 
  unique_nodes
  |> List.map (fun n -> fixed_node_longest n player_edges [] hash_table) 
  |> List.map (fun (length, _) -> length)
  |> (fun l -> get_max (List.hd l) l)

(* ########## PORTS ############ *)

(** [check_player_three_to_one player board] is [true] iff
    [player] can access a 3 : 1 port on the [board]. *)
let check_player_three_to_one player board =
  let nodes = board.nodes_occupied in 
  List.filter 
    (fun (n, p, s) -> p = player && (List.mem n three_to_one_port_nodes))
    nodes 
  <> []

(** [check_player_two_to_one player resource board] is [true] iff
    [player] can access a 2 : 1 port for [resource] on the [board]. *)
let check_player_two_to_one player resource_string board = 
  let resource = string_to_resource resource_string in 
  let nodes = board.nodes_occupied in 
  let player_nodes = 
    nodes 
    |> List.filter (fun (n, p, s) -> p = player)
    |> List.map (fun (n, _, _) -> n) in 
  let resource_ports = 
    two_to_one_port_nodes
    |> List.filter (fun (n, r) -> r = resource)
    |> List.map (fun (n, _) -> n) in 
  List.exists (fun n -> List.mem n player_nodes) resource_ports

(* ######### BOARD STRING REPRESENTATION ############ *)

let c_SINGLE_NUM_SEP = "    "
let c_DOUBLE_NUM_SEP = "   "
let c_NINE_SEP = "         "

let num_to_string num = 
  match num with
  | 2 -> c_SINGLE_NUM_SEP ^ "2" ^ c_SINGLE_NUM_SEP
  | 3 -> c_SINGLE_NUM_SEP ^ "3" ^ c_SINGLE_NUM_SEP
  | 4 -> c_SINGLE_NUM_SEP ^ "4" ^ c_SINGLE_NUM_SEP
  | 5 -> c_SINGLE_NUM_SEP ^ "5" ^ c_SINGLE_NUM_SEP
  | 6 -> c_SINGLE_NUM_SEP ^ "6" ^ c_SINGLE_NUM_SEP
  | 7 -> c_SINGLE_NUM_SEP ^ "7" ^ c_SINGLE_NUM_SEP
  | 8 -> c_SINGLE_NUM_SEP ^ "8" ^ c_SINGLE_NUM_SEP
  | 9 -> c_SINGLE_NUM_SEP ^ "9" ^ c_SINGLE_NUM_SEP
  | 10 -> c_DOUBLE_NUM_SEP ^ "10" ^ c_SINGLE_NUM_SEP
  | 11 -> c_DOUBLE_NUM_SEP ^ "11" ^ c_SINGLE_NUM_SEP
  | 12 -> c_DOUBLE_NUM_SEP ^ "12" ^ c_SINGLE_NUM_SEP
  | _ -> failwith "Cannot convert non-hex tile integer to string"

let c_TWO_SEP  = "  "
let c_THREE_SEP = "   "
let c_FOUR_SEP = "    "

let hex_resources_to_string res = 
  match res with
  | Desert -> c_TWO_SEP ^ "desert" ^ c_THREE_SEP
  | Brick -> c_THREE_SEP ^ "brick" ^ c_THREE_SEP
  | Lumber -> c_TWO_SEP ^ "lumber" ^ c_THREE_SEP
  | Ore -> c_FOUR_SEP ^ "ore" ^ c_FOUR_SEP
  | Wheat -> c_THREE_SEP ^ "wheat" ^ c_THREE_SEP
  | Wool -> c_THREE_SEP ^ "wool" ^ c_FOUR_SEP

let tile_to_position (tile : char) : int = 
  assert (tile >= 'A' && tile <= 'S');
  Char.code tile - Char.code 'A'

let c_ROBBER = " ROBBER  "
let c_EMPTY =  "         "

let rec generate_robber_list tile = 
  let position = tile_to_position tile in 
  let rec robber_list_helper position n acc = 
    if n = 26 then List.rev acc 
    else begin
      if n = position then robber_list_helper position (n + 1) (c_ROBBER :: acc)
      else robber_list_helper position (n + 1) (c_EMPTY :: acc)
    end 
  in robber_list_helper position 0 []

(* let rec list_generator filler n acc = 
   if n = 0 then acc 
   else list_generator filler (n - 1) (filler :: acc) *)

(* let empty_cities_list = 
   list_generator "#" c_NUM_NODES [] *)

let rec node_replacement n cities_list str = 
  match String.get str 0 with 
  | exception e -> ""
  | c -> begin
      if c = '>' || c = '<' 
      then List.nth cities_list n ^ node_replacement (n + 1) cities_list (String.sub str 1 (String.length str - 1))
      else if c = '\n' then "\n" ^ node_replacement n cities_list (String.sub str 1 (String.length str - 1))
      else if c = '\t' then "\t" ^ node_replacement n cities_list (String.sub str 1 (String.length str - 1))
      else if c = '\\' then "\\" ^ node_replacement n cities_list (String.sub str 1 (String.length str - 1))
      else Char.escaped c ^ node_replacement n cities_list (String.sub str 1 (String.length str - 1))
    end

let direction_chooser n1 = 
  if n1 < 13 || (n1 >= 19 && n1 < 25) || (n1 >= 31 && n1 < 37) || n1 >= 43 then begin
    n1 mod 2 = 0 
  end
  else  n1 mod 2 <> 0

let rec find_n1_position n1 counter str_list_offset = 
  match str_list_offset with 
  | [] -> failwith "Could not find n1 in offset list"
  | s :: t -> begin 
      match Str.search_forward (Str.regexp (string_of_int n1)) s 0 with
      | exception e -> find_n1_position n1 (counter + 1) t 
      | i -> 
        if direction_chooser n1 && n1 >= 10 then (i + 1, counter)
        else (i, counter)
    end 

(* let consecutive (n1_pos, row) counter str_list_offset replacement = 
   match str_list_offset with 
   | [] -> failwith "Could not find n1 in offset list"
   | s :: t -> begin 
    if counter = (row + 1) then Str.
    else s :: consecutive (n1_pos, row) (counter + 1) t
   end *)

let rec replace_position column counter replacement str = 
  match String.get str 0 with 
  | exception e -> str
  | c -> 
    if counter = column then replacement ^ (String.sub str 1 (String.length str - 1))
    else begin 
      if c = '\n' then "\n" ^ replace_position column (counter + 1) replacement (String.sub str 1 (String.length str - 1)) 
      else if c = '\t' then "\t" ^ replace_position column (counter + 1) replacement (String.sub str 1 (String.length str - 1)) 
      else if c = '\\' then "\\" ^ replace_position column (counter + 1) replacement (String.sub str 1 (String.length str - 1)) 
      else Char.escaped c ^ replace_position column (counter + 1) replacement (String.sub str 1 (String.length str - 1)) 
    end

let rec non_consecutive_replacement n1 i row1 row2 counter replacement str_list_offset = 
  match str_list_offset with 
  | [] -> str_list_offset 
  | row :: t ->
    if direction_chooser n1 then begin
      if counter = row1 
      then replace_position (i + 1) 0 replacement row :: non_consecutive_replacement n1 i row1 row2 (counter + 1) replacement t
      else if counter = row2 
      then replace_position (i + 2) 0 replacement row :: t
      else row :: non_consecutive_replacement n1 i row1 row2 (counter + 1) replacement t
    end 
    else begin
      if counter = row1 
      then replace_position (i - 1) 0 replacement row :: non_consecutive_replacement n1 i row1 row2 (counter + 1) replacement t
      else if counter = row2 
      then replace_position (i - 2) 0 replacement row :: t
      else row :: non_consecutive_replacement n1 i row1 row2 (counter + 1) replacement t
    end

let replace_run column run_length replacement str = 
  let remainder_length = (String.length str) - run_length - column - 1 in 
  (String.sub str 0 (column + 1)) ^ replacement ^ (String.sub str (column + run_length + 1) remainder_length) 

let rec consecutive_replacement n1 i row counter replacement str_list_offset = 
  match str_list_offset with 
  | [] -> str_list_offset
  | h :: t ->
    if counter = (row + 1) then replace_run i 5 replacement h :: t
    else h :: consecutive_replacement n1 i row (counter + 1) replacement t

let rec edge_replacement edges_list str_list_preset str_list_offset = 
  match edges_list with 
  | [] -> str_list_preset @ str_list_offset
  | (n1, n2, replacement) :: t->   
    assert (n1 < n2);
    let n1_col_index, row = find_n1_position n1 0 str_list_offset in 
    if n2 - n1 = 1 then 
      edge_replacement t str_list_preset
        (consecutive_replacement n1 n1_col_index row 0 replacement str_list_offset)
    else 
      edge_replacement t str_list_preset
        (non_consecutive_replacement n1 n1_col_index (row + 2) (row + 3) 0 replacement str_list_offset)

let c_P1_CONSEC = "====="
let c_P2_CONSEC = {|"""""|}
let c_P1_NONCONSEC = "="
let c_P2_NONCONSEC = {|"|}

let rec edges_to_edge_list (edges : (int * int * bool) list) : (int * int * string) list  = 
  match edges with 
  | [] -> []
  | (n1, n2, b) :: t -> 
    if b then begin
      if abs(n1 - n2) = 1 then (n1, n2, c_P1_CONSEC) :: edges_to_edge_list t
      else (n1, n2, c_P1_NONCONSEC) :: edges_to_edge_list t
    end 
    else begin
      if abs(n1 - n2) = 1 then (n1, n2, c_P2_CONSEC) :: edges_to_edge_list t
      else (n1, n2, c_P2_NONCONSEC) :: edges_to_edge_list t
    end

let rec split_list n preset list  = 
  match n, list with 
  | 0, [] -> List.rev preset , []
  | n, [] -> failwith "Cannot split list"
  | 0, h :: t -> List.rev preset , h :: t
  | n, h :: t -> split_list (n - 1) (h :: preset) t 

let hex_resource_numbers = 
  List.map 
    (fun (_, resource, num) ->
       hex_resources_to_string resource, num_to_string num) 
    tile_resource_value

let c_HEX_RESOURCES, c_HEX_NUMBERS = List.split hex_resource_numbers

let c_CITY_CHAR = "c"
let c_SETTLEMENT_CHAR = "s"
let c_EMPTY_NODE = "#"

let player_to_structure_char player char = 
  match player with 
  | true -> String.uppercase_ascii char 
  | false -> char

let structure_to_char player structure = 
  (match structure with
   | City -> c_CITY_CHAR
   | Settlement -> c_SETTLEMENT_CHAR)
  |> player_to_structure_char player

let rec check_nodes_list num nodes_occupied = 
  match nodes_occupied with 
  | [] -> None 
  | (n, p, structure) :: t ->
    if n = num then Some (p, structure)
    else check_nodes_list num t

let rec structures_to_cities_list n nodes_occupied acc = 
  if n = c_NUM_NODES + 1 then List.rev acc
  else begin
    match check_nodes_list n nodes_occupied with 
    | Some (p, structure) -> 
      structures_to_cities_list (n + 1) nodes_occupied (structure_to_char p structure :: acc)
    | None -> 
      structures_to_cities_list (n + 1) nodes_occupied (c_EMPTY_NODE :: acc) 
  end

let string_of_map robber_tile cities edges = 
  let cities_list = structures_to_cities_list 1 cities [] in 
  let edges_list = edges_to_edge_list edges in 
  let robber_list = generate_robber_list robber_tile in 
  let nums = c_HEX_NUMBERS in 
  let res = c_HEX_RESOURCES in 
  let an = List.nth nums 0 in
  let ar = List.nth res 0 in 
  let bn = List.nth nums 1 in 
  let br = List.nth res 1 in 
  let cn = List.nth nums 2 in 
  let cr = List.nth res 2 in 
  let dn = List.nth nums 3 in 
  let dr = List.nth res 3 in 
  let en = List.nth nums 4 in 
  let er = List.nth res 4 in 
  let fn = List.nth nums 5 in 
  let fr = List.nth res 5 in 
  let gn = List.nth nums 6 in 
  let gr = List.nth res 6 in 
  let hn = List.nth nums 7 in 
  let hr = List.nth res 7 in 
  let iN = List.nth nums 8 in 
  let ir = List.nth res 8 in  
  let jn = List.nth nums 9 in 
  let jr = List.nth res 9 in 
  let kn = List.nth nums 10 in 
  let kr = List.nth res 10 in 
  let ln = List.nth nums 11 in 
  let lr = List.nth res 11 in 
  let mn = List.nth nums 12 in 
  let mr = List.nth res 12 in 
  let nn = List.nth nums 13 in
  let nr = List.nth res 13 in
  let on = List.nth nums 14 in 
  let oR = List.nth res 14 in
  let pn = List.nth nums 15 in 
  let pr = List.nth res 15 in 
  let qn = List.nth nums 16 in 
  let qr = List.nth res 16 in 
  let rn = List.nth nums 17 in 
  let rr = List.nth res 17 in
  let sn = List.nth nums 18 in 
  let sr = List.nth res 18 in

  let aR = List.nth robber_list 0 in 
  let bR = List.nth robber_list 1 in 
  let cR = List.nth robber_list 2 in 
  let dR = List.nth robber_list 3 in 
  let eR = List.nth robber_list 4 in 
  let fR = List.nth robber_list 5 in 
  let gR = List.nth robber_list 6 in 
  let hR = List.nth robber_list 7 in 
  let iR = List.nth robber_list 8 in 
  let jR = List.nth robber_list 9 in 
  let kR = List.nth robber_list 10 in 
  let lR = List.nth robber_list 11 in 
  let mR = List.nth robber_list 12 in 
  let nR = List.nth robber_list 13 in 
  let oo = List.nth robber_list 14 in 
  let pR = List.nth robber_list 15 in 
  let qR = List.nth robber_list 16 in 
  let rR = List.nth robber_list 17 in 
  let sR = List.nth robber_list 18 in
  {|                               
                                  %-----%
                                 /~~~~~~~\
                                /~~~~~~~~~\
                         %-----%~~~~3:1~~~~%-----%
                        /~~~~~~~\~~~~~~~~~/~~~~~~~\
                       /~~~~~~~~~\1~~~~~2/~~~~~~~~~\
                %-----%~~~~~~~~~~*>-----<*~~~~~~~~~~%-----%
               /~~~~~~~\~~~~~~~~~/TILE A \~~~~~~~~~/~~~~~~~\
              /~~~2:1~~~\3~~~~~4/|}^an ^{|\5~~~~~6/~~~2:1~~~\
       %-----%~~~wood~~~*>-----<|} ^ar ^ {|>-----<*~~sheep~~~%-----%
      /~~~~~~~\~~~~~~~~~/TILE B \|}^aR ^{|/TILE C \~~~~~~~~~/~~~~~~~\
     /~~~~~~~~~\7~~~~*8/|}^bn^ {|\9    10/|}^cn ^{|\11*~~12/~~~~~~~~~\
    %~~~~~~~~~~~>-----<|} ^br ^ {|>-----<|} ^cr ^ {|>-----<~~~~~~~~~~~%
     \~~~~~~~~~/TILE D \|}^bR ^{|/TILE E \|}^cR ^{|/TILE F \~~~~~~~~~/
      \~~~~~13/|}^dn ^{|\14   15/|}^en ^{|\16   17/|}^fn ^{|\18~~~~~/
       %-----<|} ^dr ^ {|>-----<|} ^er ^ {|>-----<|} ^fr ^ {|>-----%
      /~~~~~~~\|}^dR ^{|/TILE G \|}^eR ^{|/TILE H \|}^fR ^{|/~~~~~~~\
     /~~~2:1~~~\19   20/|}^gn ^{|\21   22/|}^hn ^{|\23   24/~~~2:1~~~\
    %~~~brick~~*>-----<|} ^gr ^ {|>-----<|} ^hr ^ {|>-----<*~~~ore~~~~%
     \~~~~~~~~~/TILE I \|}^gR ^{|/TILE J \|}^hR ^{|/TILE K \~~~~~~~~~/
      \~~~~*25/|}^iN ^{|\26   27/|}^jn ^{|\28   29/|}^kn ^{|\30*~~~~/
       %-----<|} ^ir ^ {|>-----<|} ^jr ^ {|>-----<|} ^ kr ^ {|>-----%
      /~~~~~~~\|}^iR ^{|/TILE L \|}^jR ^{|/TILE M \|}^kR ^{|/~~~~~~~\
     /~~~~~~~~~\31   32/|}^ln ^{|\33   34/|}^mn ^{|\35   36/~~~~~~~~~\
    %~~~~~~~~~~~>-----<|} ^lr ^ {|>-----<|} ^mr ^ {|>-----<~~~~~~~~~~~%  
     \~~~~~~~~~/TILE N \|}^lR ^{|/TILE O \|}^mR ^{|/TILE P \~~~~~~~~~/
      \~~~~~37/|}^nn ^{|\38   39/|}^on ^{|\40   41/|}^pn ^{|\42~~~~~/
       %-----<|} ^nr ^ {|>-----<|} ^oR ^ {|>-----<|} ^pr ^ {|>-----%
      /~~~~~~*\|}^nR ^{|/TILE Q \|}^oo ^{|/TILE R \|}^pR ^{|/*~~~~~~\
     /~~~~~~~~~\43   44/|}^qn ^{|\45   46/|}^rn ^{|\47   48/~~~~~~~~~\
    %~~~~3:1~~~*>-----<|} ^qr ^ {|>-----<|} ^rr ^ {|>-----<*~~~3:1~~~~%
     \~~~~~~~~~/~~~~~~~\|}^qR ^{|/TILE S \|}^rR ^{|/~~~~~~~\~~~~~~~~~/
      \~~~~~~~/~~~~~~~~~\49   50/|}^sn ^{|\51   52/~~~~~~~~~\~~~~~~~/
       %-----%~~~~~~~~~~~>-----<|} ^sr ^ {|>-----<~~~~~~~~~~~%-----%
              \~~~~~~~~~/*~~~~~*\|}^sR ^{|/*~~~~~*\~~~~~~~~~/
               \~~~~~~~/~~~~~~~~~\53   54/~~~2:1~~~\~~~~~~~/
                %-----%~~~~3:1~~~~>-----<~~~grain~~~%-----%
                       \~~~~~~~~~/~~~~~~~\~~~~~~~~~/
                        \~~~~~~~/~~~~~~~~~\~~~~~~~/
                         %-----%~~~~~~~~~~~%-----%
                                \~~~~~~~~~/
                                 \~~~~~~~/
                                  %-----% |}
  |> node_replacement 0 cities_list
  |> String.split_on_char '\n'
  |> split_list 6 [] 
  |> fun (preset, offset) -> edge_replacement edges_list preset offset 

let print_map board = 
  let robber_tile = (board.robber_node |> tile_to_string |> String.get) 0 in 
  string_of_map robber_tile board.nodes_occupied board.edges_occupied
  |> List.map print_endline
  |> ignore







(* DEAD OR DEPRECATED CODE *)
(* ####### BASIC NODE LIST generator ###### *)

let rec node_status_generator n acc = 
  if n = 0 then acc
  else node_status_generator (n - 1) ((n, None) :: acc)

let node_occupancy = 
  node_status_generator c_NUM_NODES []



(* 

     type resource = string

     type tile_val = int

     type house = 
     | City
     | Settlement

     type vertex = 
     | Uninhabited
     | Player1 of house
     | Player2 of house

     type roadway = 
     | Road
     | NoRoad

     type hex = {
     hex_name : string;
     resource : resource;
     value : tile_val;
     a : vertex;
     b : vertex;
     c : vertex;
     d : vertex;
     e : vertex;
     f : vertex;
     a_neighbors : int list;
     b_neighbors : int list;
     c_neighbors : int list;
     d_neighbors : int list;
     e_neighbors : int list;
     f_neighbors : int list;
     }

     type board = hex array

     let initialize_tile (name, resource, value) = {
     hex_name = name;
     resource = resource;
     value = value;
     a = Uninhabited;
     b = Uninhabited;
     c = Uninhabited;
     d = Uninhabited;
     e = Uninhabited;
     f = Uninhabited;
     a_neighbors = [];
     b_neighbors = [];
     c_neighbors = [];
     d_neighbors = [];
     e_neighbors = [];
     f_neighbors = [];
     }

     let name_resouce_value_list = [
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ("A", "grain", 3);
     ]

     let tile_list = 
     List.map initialize_tile name_resouce_value_list

     let unconnected_board  = 
     Array.of_list tile_list

     let add_neighbors board_arr tile_num neighbor_tuple  = 
     let (a, b, c, d, e, f) = neighbor_tuple in
     let tile = board_arr.(tile_num) in 
     board_arr.(tile_num) <- {
     tile with 
     a_neighbors =  a;
     b_neighbors =  b;
     c_neighbors =  c;
     d_neighbors =  d;
     e_neighbors =  e;
     f_neighbors =  f;
     }

     let rec (--) n acc = 
     if n = 0 then acc
     else (--) (n - 1) (n :: acc)

     let tile_num_arr = (--) 19 [] |> Array.of_list

     let neighbor_tuples_list = [
     ([], [2], [3; 5], [2; 5], [2], []); (* A *)
     ([1], [1; 5], [5; 7], [4; 7], [4], []); (* B *)
     ([], [6], [6; 8], [5; 8], [1; 5], [1]); (* C *)
     ([2], [2; 7], [9; 7], [9], [], []); (* D *)
     ([1; 3], [3; 8], [8; 10], [7; 10], [2; 7], [1; 2]); (* E *)
     ([], [], [11], [8; 11], [3; 8], [3]); (* F *)
     ([2; 5], [5; 10], [10; 12], [9; 12], [4; 9], [2; 4]); (* G *)
     ([3; 6], [6; 11], [11; 13], [10; 13], [5; 10], [3; 5]); (* H *)
     ([4; 7], [7; 12], [12; 14], [14], [], [4]); (* I *)
     ([5; 8], [8; 13], [13; 15], [12; 15], [7; 12], [5; 7]); (* J *)
     ([6], [], [16], [13; 16], [8; 13], [6; 8]); (* K *)
     ([7; 10], [10; 15], [15; 17], [14; 17], [9; 14], [7; 9]); (* L *)
     ([8; 11], [11; 16], [16; 18], [15; 18], [10; 15], [9; 10]); (* M *)
     ([9; 12], [12; 17], [17], [], [], [9]); (* N *)
     ([10; 13], [13; 18], [18; 19], [17; 19], [12; 17], [10; 12]); (* O *)
     ([11], [], [], [18], [13; 18], [11; 13]); (* P *)
     ([12; 15], [15; 19], [19], [], [14], [12; 14]); (* Q *)
     ([13; 16], [16], [], [19], [19; 15], [13; 15]); (* R *)
     ([15; 18], [18], [], [], [17], [17; 15]); (* S *)
     ]

     let index_neighbor_tuples_array = 
     neighbor_tuples_list 
     |> List.map 
     (fun elt ->
       let (a, b, c, d, e, f) = elt in 
       let a' = List.map (fun n -> n - 1) a |> List.sort_uniq compare in 
       let b' = List.map (fun n -> n - 1) b |> List.sort_uniq compare in 
       let c' = List.map (fun n -> n - 1) c |> List.sort_uniq compare in 
       let d' = List.map (fun n -> n - 1) d |> List.sort_uniq compare in 
       let e' = List.map (fun n -> n - 1) e |> List.sort_uniq compare in 
       let f' = List.map (fun n -> n - 1) f |> List.sort_uniq compare in 
       (a', b', c', d', e', f')
     ) 
     |> Array.of_list

     let init_board = 
     Array.map2 
     (fun tile_num neigh_tup -> add_neighbors unconnected_board tile_num neigh_tup)
     tile_num_arr 
     index_neighbor_tuples_array

     let tile_name_to_value c =
     Char.code c - Char.code 'A'

     let value_to_tile_name v = 
     Char.chr (v + Char.code 'A')

     type location = 
     | UpperRight
     | Right
     | LowerRight
     | LowerLeft
     | Left
     | UpperLeft

     type player = 
     | P1
     | P2
     (* 
     let convert_location loc = 
     match loc with
     | UpperRight -> 
     | Right
     | LowerRight
     | LowerLeft
     | Left
     | UpperLeft *)

     let rec check_equivalent_vertex add_func original_location surround_list board = 
     match surround_list with
     | [] -> board
     | h :: t ->
     add_func (value_to_tile_name h) 

     let rec add_settlement tile_name location player board = 
     let value = tile_name_to_value tile_name in 
     let tile = board.(value) in 
     match location, player  with
     | UpperRight, P1 ->
     board.(value) <- {
      tile with
      a = Player1 (Settlement)
     }; board
     | UpperRight, P2 ->
     board.(value) <- {
      tile with
      a = Player2 (Settlement)
     }; board
     | Right, P1 ->
     board.(value) <- {
      tile with
      b = Player1 (Settlement)
     }; board
     | Right, P2 ->
     board.(value) <- {
      tile with
      b = Player2 (Settlement)
     }; board
     | LowerRight, P1 ->
     board.(value) <- {
      tile with
      c = Player1 (Settlement)
     }; board
     | LowerRight, P2 ->
     board.(value) <- {
      tile with
      c = Player2 (Settlement)
     }; board
     | LowerLeft, P1 ->
     board.(value) <- {
      tile with
      d = Player1 (Settlement)
     }; board
     | LowerLeft, P2 ->
     board.(value) <- {
      tile with
      d = Player2 (Settlement)
     }; board
     | Left, P1 ->
     board.(value) <- {
      tile with
      e = Player1 (Settlement)
     }; board
     | Left, P2 ->
     board.(value) <- {
      tile with
      e = Player2 (Settlement)
     }; board
     | UpperLeft, P1 ->
     board.(value) <- {
      tile with
      f = Player1 (Settlement)
     }; board
     | UpperLeft, P2 ->
     board.(value) <- {
      tile with
      f = Player2 (Settlement)
     }; board

   *)