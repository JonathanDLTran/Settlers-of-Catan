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

(* ####### BOARD INSTANTIATION ########## *)

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
  (A, Wheat, 9);
  (B, Lumber, 8);
  (C, Wheat, 12);
  (D, Brick, 5);
  (E, Lumber, 11);
  (F, Ore, 10);
  (G, Ore, 3);
  (H, Brick, 11);
  (I, Wheat, 6);
  (J, Desert, 0);
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

(* ########NODE NEIGHBOR HELPERS ######## *)

let special_nodes = [
  1; 2; 3; 6; 7; 12; 13; 18; 25; 30; 37; 42; 43; 48; 49; 52; 53; 54
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
    | 6 -> [5; 11]
    | 7 -> [8; 13]
    | 12 -> [11; 18]
    | 13 -> [7; 19]
    | 18 -> [12; 24]
    | 25 -> [19; 31]
    | 30 -> [24; 26]
    | 37 -> [31; 43]
    | 42 -> [36; 48]
    | 43 -> [37; 44]
    | 48 -> [42; 47]
    | 49 -> [44; 50]
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
  | Desert -> failwith "resource cannot be desert"

(** [structure_to_num_cards structure] converts a structure
    to the equivalent number of card resources that structure produces.  *)
let structure_to_num_cards structure = 
  match structure with 
  | Settlement -> 1
  | City -> 2

(** [mem_nodes_occupied node player nodes_occupied] is the number
    of cards a player would get given [node] for [player].  *)
let rec mem_nodes_occupied node player nodes_occupied = 
  match nodes_occupied with 
  | [] -> 0
  | (n, p, structure) :: t ->
    if n = node && p = player then structure_to_num_cards structure 
    else mem_nodes_occupied node player t

(** [neighbors_list_to_resources player resource_hoard board neighbors] 
    is the [resource hoard] for the [player] based on [neighbors] 
    and the game [board]. *)
let rec neighbors_list_to_resources player resource_hoard board neighbors = 
  match neighbors with 
  | [] -> resource_hoard
  | (node, resource) :: t ->  
    let num_cards = mem_nodes_occupied node player board.nodes_occupied in 
    let new_hoard = update_resource_hoard resource num_cards resource_hoard in 
    neighbors_list_to_resources player new_hoard board t

(** [add_resources roll board] is the pair of resource hoards
    for the resources that player1 and player2 would pick up
    upon the [roll] on the game [board] at the current state.  *)
let get_resources roll board = 
  roll 
  |> roll_to_tile 
  |> tile_to_neighbors
  |> (fun neighbors ->
      (neighbors_list_to_resources true empty_resource_hoard board neighbors,
       neighbors_list_to_resources false empty_resource_hoard board neighbors))

(* ############### EDGE STUFF ########### *)

(** [connected node1 node2 player edges] 
    is [true] iff either [node1] or [node2] connects
    to the rest of [edges]. *)
let connected node1 node2 player edges = 
  if List.filter (fun (n1, n2, p) -> (n1 = node1 || n2 = node2) && player = p) edges <> [] then true 
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
  else if List.filter (fun (n1, n2, _) -> n1 = node1 && n2 = node2) edges <> [] then false (* node not occupied *)
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
  else if List.filter (fun (n1, n2, _) -> n1 = node1 && n2 = node2) board.edges_occupied <> [] then (Failure (UnconnectedErr, board)) (* node not occupied *)
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
  else if List.filter (fun (n1, n2, _) -> n1 = node1 && n2 = node2) board.edges_occupied <> [] then (Failure (UnconnectedErr, board)) (* node not occupied *)
  else if not (check_edge_connected_to_settlement player node1 node2 board.nodes_occupied) then (Failure (UnconnectedErr, board)) (* node connected to another node *)
  else Success {board with edges_occupied = (node1, node2, player) :: board.edges_occupied}





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