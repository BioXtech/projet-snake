open_graph(700,700);;
#use "CPinter_sn.ml";;
#use "snake1.ml";;

type t_play = { dt : float ref ; sn : t_snake ref ; mat : t_matrix ; score : int ref };;


(** le score sera calculé fonction du temps 
    @param pl le plateau de jeu
    @return le score actualisé
    @author Duc
    @since 2.0
 *)

let increase_score(pl : t_play) : int =
  pl.score := (!(pl.score)) + 10;
  (!(pl.score));
;;


(** Affiche le score dans la fenêtre graphique en dessous de la mattrice de jeu en faisant bien attention qu'ils ne soit pas placé dans la matrice de jeu 
    @author Duc
    @sice 2.0
 *)
let set_score () : unit =
  set_color(color_of_value(FRAME));
  moveto(mymatrix_dx() * mydilation_x() /2 ,mymatrix_dy() * mydilation_y()/10);
  draw_string("Score :"^0);
;;

(** Affiche le résultat à la suite de la fonction set_score() 
    @param pl le plateau de jeu
    @author Duc
    @since 2.0
*)
let display_score(pl : t_play ) : unit  =
  myfill_rect(0,0,mymatrix_dx() * mydilation_x() /2, mymatrix_dy() * mydilation_y()/10);
  set_score();
  draw_string(string_of_int(increase_score(pl)));
;;





 




