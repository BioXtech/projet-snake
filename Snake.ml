#use "CPinter_sn.ml";;
let mytranslation_x() : int =
  100;;

let mytranslation_y() : int =
  100;;

let mydilation_x() : int =
  5;;

let mydilation_y() : int =
  5;;

let mygraphic_x(x : int ) : int =
  mytranslation_x() + x;;

let mygraphic_y(y : int ) : int =
  mytranslation_y() + y ;;

(* Type et fonction de base *)

type t_point = {x : int ; y : int};;
type t_direction = UP | DOWN | RIGHT | LEFT;;
type t_position = {pt : t_point ; dir : t_direction};;
type t_value = EMPTY | SNAKE | FRAME | PROBLEM ;;

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



let mymatrix_dx() : int =
  500;;

let mymatrix_dy() : int =
  500;;


type t_matrix = t_value matrix;;
type t_snake = t_position list;;
type t_play = {dt : float ref; sn : t_snake ref; mat : t_matrix};;

let mydt_init() : float =
  0.00;;

let mydt_acc() : float =

;;

let mydt_ratio() : float =

;;

let mysnake_length_init() : int =
3
;;

let mysnake_position_init () : t_point =
  {x = 200; y = 200};;

