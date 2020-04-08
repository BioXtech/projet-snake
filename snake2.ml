open_graph(700,700);;
#use "CPinter_sn.ml";;
#use "snake1.ml";;

type t_play = { dt : float ref ; sn : t_snake ref ; mat : t_matrix ; score : int ref };;


(** le score sera calculé fonction du temps *)


let increase_score(pl : t_play) : int =
  pl.score := (!(pl.score)) + 10;
  (!(pl.score));
;;


(** Affiche le score dans la fenêtre graphique en dessous de la mattrice de jeu en faisant bien attention qu'ils ne soit pas placé dans la matrice de jeu *)
let set_score () : unit =
  set_color(black);
  moveto(mymatrix_dx() * mydilation_x() /2 ,mymatrix_dy() * mydilation_y()/10);
  draw_string("Score :");
;;

(** Affiche le résultat à la suite de la fonction set_score() *)
let display_score(pl : t_play ) : unit  =
  set_score();
  draw_string(string_of_int(increase_score(pl)));
;;

  




