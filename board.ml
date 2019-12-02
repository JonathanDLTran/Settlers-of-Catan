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

let convert_location loc = 
  match loc with
  | UpperRight -> 
  | Right
  | LowerRight
  | LowerLeft
  | Left
  | UpperLeft

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







