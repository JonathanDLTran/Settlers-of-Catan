open Pregame_board


type game_state = 
  | Pregame
  | Ingame
  | Victory
  | Quit


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

let () = () |> generate_custom |> print_endline