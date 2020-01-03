let standard = 
  {|               
                                  >-----<
                                 /~~~~~~~\
                                /~~~~~~~~~\
                         >-----<~~~~3:1~~~~>-----<
                        /~~~~~~~\~~~~~~~~~/~~~~~~~\
                       /~~~~~~~~~\*~~~~~*/~~~~~~~~~\
                >-----<~~~~~~~~~~~>-----<~~~~~~~~~~~>-----<
               /~~~~~~~\~~~~~~~~~/       \~~~~~~~~~/~~~~~~~\
              /~~~2:1~~~\~~~~~~~/    8    \~~~~~~~/~~~2:1~~~\
       >-----<~~~wood~~~*>-----<   wood    >-----<*~~sheep~~~>-----<
      /~~~~~~~\~~~~~~~~~/       \         /       \~~~~~~~~~/~~~~~~~\
     /~~~~~~~~~\~~~~~~*/   11    \       /    4    \*~~~~~~/~~~~~~~~~\
    <~~~~~~~~~~~>-----<   brick   >-----<   brick   >-----<~~~~~~~~~~~>
     \~~~~~~~~~/       \         /       \         /       \~~~~~~~~~/
      \~~~~~~~/    3    \       /    9    \       /    9    \~~~~~~~/
       >-----<   grain   >-----<   wood    >-----<   stone   >-----<
      /~~~~~~~\         /       \         /       \         /~~~~~~~\
     /~~~2:1~~~\       /    8    \       /         \       /~~~2:1~~~\
    <~~~brick~~*>-----<   sheep   >-----<   desert  >-----<*~~~ore~~~~>
     \~~~~~~~~~/       \         /       \  ROBBER /       \~~~~~~~~~/
      \~~~~~~*/   10    \       /    6    \       /   10    \*~~~~~~/
       >-----<   wood    >-----<   brick   >-----<   sheep   >-----<
      /~~~~~~~\         /       \         /       \         /~~~~~~~\
     /~~~~~~~~~\       /   12    \       /    2    \       /~~~~~~~~~\
    <~~~~~~~~~~~>-----<   grain   >-----<   stone   >-----<~~~~~~~~~~~>  
     \~~~~~~~~~/       \         /       \         /       \~~~~~~~~~/
      \~~~~~~~/    6    \       /    4    \       /   12    \~~~~~~~/
       >-----<   grain   >-----<   sheep   >-----<   brick   >-----<
      /~~~~~~*\         /       \         /       \         /*~~~~~~\
     /~~~~~~~~~\       /   11    \       /    5    \       /~~~~~~~~~\
    <~~~~3:1~~~*>-----<   wood    >-----<   grain   >-----<*~~~3:1~~~~>
     \~~~~~~~~~/~~~~~~~\         /       \         /~~~~~~~\~~~~~~~~~/
      \~~~~~~~/~~~~~~~~~\       /    5    \       /~~~~~~~~~\~~~~~~~/
       >-----< ~~~~~~~~~~>-----<   sheep   >-----<~~~~~~~~~~~>-----<
              \~~~~~~~~~/*~~~~~*\         /*~~~~~*\~~~~~~~~~/
               \~~~~~~~/~~~~~~~~~\       /~~~2:1~~~\~~~~~~~/
                >-----<~~~~3:1~~~~>-----<~~~grain~~~>-----<
                       \~~~~~~~~~/~~~~~~~\~~~~~~~~~/
                        \~~~~~~~/~~~~~~~~~\~~~~~~~/
                         >-----<~~~~~~~~~~~>-----<
                                \~~~~~~~~~/
                                 \~~~~~~~/
                                  >-----< |}

type hex_numbers = 
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Eleven
  | Twelve

let c_HEX_NUMBERS = [
  Two;
  Three; Three;
  Four; Four;
  Five; Five;
  Six; Six;
  Seven;
  Eight; Eight;
  Nine; Nine;
  Ten; Ten;
  Eleven; Eleven;
  Twelve;
]

let c_SINGLE_NUM_SEP = "    "
let c_DOUBLE_NUM_SEP = "   "
let c_NINE_SEP = "         "

let hex_num_to_string num = 
  match num with
  | Two -> c_SINGLE_NUM_SEP ^ "2" ^ c_SINGLE_NUM_SEP
  | Three -> c_SINGLE_NUM_SEP ^ "3" ^ c_SINGLE_NUM_SEP
  | Four -> c_SINGLE_NUM_SEP ^ "4" ^ c_SINGLE_NUM_SEP
  | Five -> c_SINGLE_NUM_SEP ^ "5" ^ c_SINGLE_NUM_SEP
  | Six -> c_SINGLE_NUM_SEP ^ "6" ^ c_SINGLE_NUM_SEP
  | Seven -> c_SINGLE_NUM_SEP ^ "7" ^ c_SINGLE_NUM_SEP
  | Eight -> c_SINGLE_NUM_SEP ^ "8" ^ c_SINGLE_NUM_SEP
  | Nine -> c_SINGLE_NUM_SEP ^ "9" ^ c_SINGLE_NUM_SEP
  | Ten -> c_DOUBLE_NUM_SEP ^ "10" ^ c_SINGLE_NUM_SEP
  | Eleven -> c_DOUBLE_NUM_SEP ^ "11" ^ c_SINGLE_NUM_SEP
  | Twelve -> c_DOUBLE_NUM_SEP ^ "12" ^ c_SINGLE_NUM_SEP

type hex_resource = 
  | Desert
  | Brick
  | Lumber
  | Ore
  | Wheat
  | Wool

let c_HEX_RESOURCES = [
  Desert; 
  Brick; Brick; Brick; 
  Ore; Ore; Ore;
  Wool; Wool; Wool; Wool;
  Wheat; Wheat; Wheat; Wheat;
  Lumber; Lumber; Lumber; Lumber;
]

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

(** [shuffle lst] is a random permutation of [lst]. *)
let shuffle (lst : 'a list) : 'a list =
  QCheck.Gen.(generate1 (shuffle_l lst))

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
  in  robber_list_helper position 0 []

(* let rec generate_numbers n acc = 
   if n = 55 then List.rev acc 
   else  *)

let c_NUM_NODES = 54

let rec list_generator filler n acc = 
  if n = 0 then acc 
  else list_generator filler (n - 1) (filler :: acc)

let empty_cities_list = 
  list_generator "C" c_NUM_NODES []

let rec node_replacement n cities_list str = 
  match String.get str 0 with 
  | exception e -> ""
  | c -> 
    if c = '>' || c = '<' 
    then List.nth cities_list n ^ node_replacement (n + 1) cities_list (String.sub str 1 (String.length str - 1))
    else if c = '\n' then "\n" ^ node_replacement n cities_list (String.sub str 1 (String.length str - 1))
    else if c = '\t' then "\t" ^ node_replacement n cities_list (String.sub str 1 (String.length str - 1))
    else if c = '\\' then "\\" ^ node_replacement n cities_list (String.sub str 1 (String.length str - 1))
    else Char.escaped c ^ node_replacement n cities_list (String.sub str 1 (String.length str - 1))

let direction_chooser n1 = 
  if n1 < 13 || (n1 >= 19 && n1 < 25) || (n1 >= 31 && n1 < 37) || n1 >= 43 then begin
    n1 mod 2 = 0 
  end
  else 
    n1 mod 2 <> 0

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

let generate_custom robber_tile edges = 
  let edges_list = edges_to_edge_list edges in 
  let robber_list = generate_robber_list robber_tile in 
  let nums = shuffle c_HEX_NUMBERS |> List.map hex_num_to_string in 
  let res = shuffle c_HEX_RESOURCES |> List.map hex_resources_to_string in 
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
  |> node_replacement 0 empty_cities_list
  |> String.split_on_char '\n'
  |> split_list 6 [] 
  |> fun (preset, offset) -> edge_replacement edges_list preset offset 



(* 
let generate_custom1 robber_tile edge_array = 
  let robber_list = generate_robber_list robber_tile in 
  let nums = shuffle c_HEX_NUMBERS |> List.map hex_num_to_string in 
  let res = shuffle c_HEX_RESOURCES |> List.map hex_resources_to_string in 
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

  (* let e12 = (Array.get edge_array 1 |> Array.get) 2 in  *)
  let e12 = List.nth edge_array 0 in 
  let e14 = List.nth edge_array 1 in 
  let e15 = List.nth edge_array 2 in 
  {|                               
                                  %-----%
                                 /~~~~~~~\
                                /~~~~~~~~~\
                         %-----%~~~~3:1~~~~%-----%
                        /~~~~~~~\~~~~~~~~~/~~~~~~~\
                       /~~~~~~~~~\*~~~~~*/~~~~~~~~~\
                %-----%~~~~~~~~~~1>|}^e12^{|2~~~~~~~~~~%-----%
               /~~~~~~~\~~~~~~~~~|}^e14^{|TILE A |}^e15^{|~~~~~~~~~/~~~~~~~\
              /~~~2:1~~~\~~~~~~4|}^e14^{||}^an ^{||}^e15^{|5~~~~~~/~~~2:1~~~\
       %-----%~~~wood~~3*>-----<|} ^ar ^ {|>-----<*6~sheep~~~%-----%
      /~~~~~~~\~~~~~~~~~/TILE B \|}^aR ^{|/TILE C \~~~~~~~~~/~~~~~~~\
     /~~~~~~~~~\~~~~~8*/|}^bn^ {|\9    10/|}^cn ^{|\*11~~~~/~~~~~~~~~\
    %~~~~~~~~~~7>-----<|} ^br ^ {|>-----<|} ^cr ^ {|>-----<12~~~~~~~~~%
     \~~~~~~~~~/TILE D \|}^bR ^{|/TILE E \|}^cR ^{|/TILE F \~~~~~~~~~/
      \~~~~~~~/|}^dn ^{|\14   15/|}^en ^{|\16   17/|}^fn ^{|\~~~~~~~/
       %---13<|} ^dr ^ {|>-----<|} ^er ^ {|>-----<|} ^fr ^ {|>18---%
      /~~~~~~~\|}^dR ^{|/TILE G \|}^eR ^{|/TILE H \|}^fR ^{|/~~~~~~~\
     /~~~2:1~~~\19   20/|}^gn ^{|\21   22/|}^hn ^{|\23   24/~~~2:1~~~\
    %~~~brick~~*>-----<|} ^gr ^ {|>-----<|} ^hr ^ {|>-----<*~~~ore~~~~%
     \~~~~~~~~~/TILE I \|}^gR ^{|/TILE J \|}^hR ^{|/TILE K \~~~~~~~~~/
      \~~~~~~*/|}^iN ^{|\26   27/|}^jn ^{|\28   29/|}^kn ^{|\*~~~~~~/
       %---25<|} ^ir ^ {|>-----<|} ^jr ^ {|>-----<|} ^ kr ^ {|>30---%
      /~~~~~~~\|}^iR ^{|/TILE L \|}^jR ^{|/TILE M \|}^kR ^{|/~~~~~~~\
     /~~~~~~~~~\31   32/|}^ln ^{|\33   34/|}^mn ^{|\35  36/~~~~~~~~~\
    %~~~~~~~~~~~>-----<|} ^lr ^ {|>-----<|} ^mr ^ {|>-----<~~~~~~~~~~~%  
     \~~~~~~~~~/TILE N \|}^lR ^{|/TILE O \|}^mR ^{|/TILE P \~~~~~~~~~/
      \~~~~~~~/|}^nn ^{|\38   39/|}^on ^{|\40   41/|}^pn ^{|\~~~~~~~/
       %---37<|} ^nr ^ {|>-----<|} ^oR ^ {|>-----<|} ^pr ^ {|>42---%
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
 *)
