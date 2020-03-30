open_graph(700,700);;
#use "CPinter_sn.ml";;
#use "snake1.ml";;

type t_play = { dt : float ref ; sn : t_snake ref ; mat : t_matrix ; score : int };;


(** le score sera calculé fonction du temps et des bonus *)


let calcul_score() : unit =
  let moved : t_position * t_value = compute_move() in
  while (!(move : bool)
         do
                                               

let set_score () : unit =
  set_color(black);
  moveto(250,50);
  draw_string("Score :");
;;




