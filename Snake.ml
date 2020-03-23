#use "CPinter_sn.ml";;
open_graph(700,700);;

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
  mytranslation_x() + x * mydilation_x()
;;

(** Retourne la coordonnee y d'un pixel *)
let mygraphic_y(y : int) : int =
  mytranslation_y() + y * mydilation_y()
;;

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

type t_point = {x : int ; y : int};;
type t_direction = UP | DOWN | RIGHT | LEFT;;
type t_position = {pt : t_point ; dir : t_direction};;
type t_value = EMPTY | SNAKE | FRAME | PROBLEM ;;

let color_of_value(x : t_value ) : t_color =
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
  100;;

let mymatrix_dy() : int =
  100;;


type t_matrix = t_value matrix;;
type t_snake = t_position list;;
type t_play = {dt : float ref; sn : t_snake ref; mat : t_matrix};;

let mydt_init() : float =
  0.1;;

let mydt_acc() : float =
  3.0
;;

let mydt_ratio() : float =
  0.1
;;

let mysnake_length_init() : int =
3
;;

let mysnake_position_init () : t_point =
  {x = 20; y = 20}
;;

(** Dessine le cadre autour de la matrice de jeu *)
let draw_frame() : unit =
  myfill_rect(0, 0, 1 ,mymatrix_dy());
  myfill_rect(0, 0, mymatrix_dx(), 1);
  myfill_rect(0, mymatrix_dy(), mymatrix_dx() + 1 ,1);
  myfill_rect(mymatrix_dx(), 0, 1 ,mymatrix_dy() + 1);
;;

(** Fonction auxiliaire pour dessiner le serpent *)
let rec draw_whole_snake_aux(s : t_snake) : unit =
  if s = []
  then ()
  else
    (
      myplot((fst(s)).pt.x,(fst(s)).pt.y);
      draw_whole_snake_aux(rem_fst(s));
    )
;;

(** Dessine la totalite du serpent *)
let draw_whole_snake(s : t_snake) : unit =
  set_color(Graphics.green);
  draw_whole_snake_aux(s);
;;

(** Fonction auxilliaire de init_snake() *)
let rec init_snake_aux(i : int) : t_snake =
  if i = 1
  then [{pt = {x = (mysnake_position_init()).x;
               y = (mysnake_position_init()).y}; dir = LEFT}]
  else add_lst(init_snake_aux(i - 1),{pt = {x = (mysnake_position_init()).x -1 + i;
                      y = (mysnake_position_init()).y} ; dir = LEFT})

(** Initialise le serpent au debut du jeu *)
let init_snake() : t_snake =
  init_snake_aux(mysnake_length_init())
;;

(** Initialise la matrice de jeu en EMPTY (en blanc)*)
let init_matrix() : t_matrix =
  mat_make(mymatrix_dx(),mymatrix_dy(),EMPTY)
;;

(*voir pour l'efficacite*)
(** Insere les positions du t_snake dans la matrice de jeu globale *)
let init_snake_matrix() : t_snake * t_matrix =
  let snake : t_snake = init_snake() and game_matrix : t_matrix = init_matrix() in
  (
    for i = 0 to len(snake) - 1
    do
      game_matrix.((nth(snake,i)).pt.x).((nth(snake,i)).pt.y) <- SNAKE;
    done;
    (snake,game_matrix);
  )
;;

(** Initialise le plateau de jeu avec le cadre et le serpent*)
let init_play() : t_play =
  draw_frame();
  draw_whole_snake(init_snake());
  {dt = {contents = mydt_init()}; sn = {contents = init_snake()}; mat = init_matrix()}
;;

(** Retourne la position mise a jour en fonction de la direction en entrée.
   2 parametres:
   - pos: position de la case du snake de type t_position
   - d: direction future de la case *)
let compute_new_position(pos, d : t_position * t_direction) : t_position =
  let x : int ref = ref pos.pt.x and y : int ref = ref pos.pt.y in
  (
    if d = UP
    then y := !y + 1
    else
      if d = DOWN
      then y := !y - 1
      else
        if d = LEFT
        then x := !x - 1
        else x := !x + 1;
      {pt = {x = !x; y = !y}; dir = pos.dir};
    )
;;

let compute_move(pos, dir, m : t_position * t_direction * t_matrix) : t_position * t_value =
  let pos
