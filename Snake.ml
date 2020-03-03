#use "CPinter_sn.ml";;

(** Retourne le coefficiant de translation sur x *)
let mytranslation_x() : int =
  100
;;

(** Retourne le coefficiant de translation sur y *)
let mytranslation_y() : int =
  100
;;

(** Retourne le coefficiant de translation sur x *)
let mydilation_x() : int =
  5
;;

(** Retourne le coefficant de translation sur y *)
let mydilation_y() : int =
  5
;;

(** Retourne la coordonnee x d'un pixel *)
let mygraphic_x(x : int) : int =
  mytranslation_x() + x
;;

(** Retourne la coordonnee y d'un pixel *)
let mygraphic_y(y : int) : int =
  mytranslation_y() + y
;;

(** Affiche dans la fenetre d'affichage un rectangle correspondant a un pixel donne en parametre
    2 parametres :
    - x : coordonnee sur x du pixel
    - y : coordonnee sur y du pixel *)
let myplot(x,y : int * int) : unit =
  open_graph(700,700);
  fill_rect(mygraphic_x(x),mygraphic_y(y),mydilation_x(),mydilation_y());
  wait(5);
  close_graph();
;;


(** Trace dans la fenetre d'affichage le rectangle vide
    4 parametres :
    - px, py : coordonnee du point en bas a gauche
    - dx, dy : longueurs des cotes *)
let myfill_rect(px, py, dx, dy : int * int * int * int) : unit =
  open_graph(700,700);
  draw_rect(mygraphic_x(px), mygraphic_y(py),mydilation_x() * dx,mydilation_y() * dy);
  wait(5);
;;
