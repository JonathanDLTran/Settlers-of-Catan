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

let generate_custom robber_tile = 
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
                       /~~~~~~~~~\*~~~~~*/~~~~~~~~~\
                %-----%~~~~~~~~~~1>-----<2~~~~~~~~~~%-----%
               /~~~~~~~\~~~~~~~~~/TILE A \~~~~~~~~~/~~~~~~~\
              /~~~2:1~~~\~~~~~~4/|}^an ^{|\5~~~~~~/~~~2:1~~~\
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

