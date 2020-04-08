open_graph(700,700);;
#use "CPinter_sn.ml";;
#use "snake1.ml";;

type t_play = { dt : float ref ; sn : t_snake ref ; mat : t_matrix ; score : int };;


(** le score sera calculé fonction du temps *)


let calcul_score(score : int) : int =
  score + 10;
;;


(** Affiche dans la fenêtre graphique en dessous du jeu le score *)
let set_score () : unit =
  set_color(black);
  moveto(mymatrix_dx() * mydilation_x() /2 ,mymatrix_dy() * mydilation_y()/10);
  draw_string("Score :"^(pl.score));
;;






