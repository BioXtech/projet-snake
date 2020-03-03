#use "CPinter_sn.ml";;
(** Ouvre le graphique *)
open_graph(700,700);;
(** Retourne le coefficiant de translation sur x *)
let mytranslation_x() : int =
  100;;

(** Retourne le coefficiant de translation sur y *)
let mytranslation_y() : int =
  100;;

(** Retourne le coefficiant de translation sur x *)
let mydilation_x() : int =
  5;;

(** Retourne le coefficant de translation sur y *)
let mydilation_y() : int =
  5;;

(** Retourne la coordonnee x d'un pixel *)
let mygraphic_x(x : int ) : int =
  mytranslation_x() + x;;

(** Retourne la coordonnee y d'un pixel *)
let mygraphic_y(y : int ) : int =
  mytranslation_y() + y ;;

(** Affiche dans la fenetre d'affichage un rectangle correspondant a un pixel donne en parametre
    2 parametres :
    - x : coordonnee sur x du pixel
    - y : coordonnee sur y du pixel *)
let myplot(x,y : int * int) : unit =
  fill_rect(mygraphic_x(x),mygraphic_y(y),mydilation_x(),mydilation_y());
;;

(** Trace dans la fenetre d'affichage le rectangle vide
    4 parametres :
    - px, py : coordonnee du point en bas a gauche
    - dx, dy : longueurs des cotes *)
let myfill_rect(px, py, dx, dy : int * int * int * int) : unit =
  fill_rect(mygraphic_x(px), mygraphic_y(py),mydilation_x() * dx,mydilation_y() * dy);
;;

(* Type et fonction de base *)
(** Represente les coordonnees d'un point *)
type t_point = {x : int ; y : int};;

(** Represente la direction que prend le serpent *)
type t_direction = UP | DOWN | RIGHT | LEFT;;

(** Represente la position du serpent et la direction*)
type t_position = {pt : t_point ; dir : t_direction};;

(** Reprensente la valeur d'une case *)
type t_value = EMPTY | SNAKE | FRAME | PROBLEM ;;


(** En fonction de la valeur de la case, donne la couleur correspondante *)
let fonction color_of_value(x : t_value ) : t_color =
  if x = PROBLEM
  then Graphics.black 
  else
    if x = FRAME
    then Graphics.red 
    else
      if x = SNAKE
      then Graphics.green 
      else Graphics.white ;;


(** Represente la longueur de la matrice en x*)
let mymatrix_dx() : int =
  500;;

(** Represente la longueur de la matrice en y *)
let mymatrix_dy() : int =
  500;;


type t_matrix = t_value matrix;;
type t_snake = t_position list;;
type t_play = {dt : float ref; sn : t_snake ref; mat : t_matrix};;
(** Represente la vitesse initiale du serpent *)
let mydt_init() : float =
  0.0;;
(** Reprensente  l'intervalle entre deux mouvements *)
let mydt_acc() : float =
3.0
;;
(** Reprensente le ratio entre la nouvelle position et la pr�cedente*)
let mydt_ratio() : float =
0.1
;;
(** Represente la longueur du serpent au point initiale *)
let mysnake_length_init() : int =
3
;;
(** Represente les coordonn�es de la position initiale du serpent *)
let mysnake_position_init () : t_point =
  {x = 200; y = 200};;

(** Dessine le cadre autour de la matrice de jeu *)
let draw_frame() : unit =
  for i = 0 to 4
  do
    draw_rect(0 -i+100,0-i+100,(99+i/5)*5,5*(99+i/5));
    
  done;;
    
let init_snake() : t_snake =
  
 
;;


  

