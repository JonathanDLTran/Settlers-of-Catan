open Pregame
open Game
open Board_settings

let opening_msg () = 
  ANSITerminal.(print_string [cyan; Bold;] "\nWelcome to Settlers of Catan!");
  ANSITerminal.(print_string [cyan; Bold;] "\nWritten by Jonathan Tran (jdt98) in OCaml")

let () = 
  ()
  |> set_screen_size
  |> opening_msg
  |> instantiate_pregame 
  |> instantiate_game

(* open Pregame_board

   type board = string list
   type player = Player.t

   type game_state = 
   | Pregame of board * player * player
   | Ingame of board * player * player
   | Victory
   | Quit

   let (>>>) f state = 
   match state with     
   | Pregame (b, p1, p2) -> f b p1 p2
   | Ingame (b, p1, p2) -> f b p1 p2
   | Victory -> Victory
   | Quit -> Quit

   (* let initialize_game = failwith "unimplemented" *)

   let start_game b p1 p2 = 
   ANSITerminal.(print_string [green]
                  "\n\nWelcome to Settlers of Catan.\n");
   Pregame (b, p1, p2)

   let print_pregame_start_msg = 
   ANSITerminal.(print_string [green] "\nThis is the pregame phase. We will place two settlements and two roads per player. ")



   (* http://boardgamestogo.com/settlers_pbem.htm *)
   let print_hexagon () = 
   print_endline {|               
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

   let () = print_hexagon ()

   let edges = 
   [(1, 2, true);
   (21, 22, false); 
   (3, 4, false); 
   (34, 40, false);
   (33, 39, true); 
   (51, 52, true); 
   (7, 13, true);
   (1, 4, false);
   (2, 5, false);
   (24, 30, true);
   (30, 36, false);
   (37, 43, true);
   (32, 38, false);
   (26, 32, true);
   (19, 25, false);
   (27, 33, true);
   (26, 27, false);
   (38, 39, true);
   (19, 20, true);
   (47, 48, false);]
   let () = generate_custom 'J' edges |> List.map print_endline |> ignore *)