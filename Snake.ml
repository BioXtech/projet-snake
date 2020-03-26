#use "CPinter_sn.ml";;
#use "CPsnake_misc.ml";;
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
  fill_rect(mygraphic_x(px), mygraphic_y(py),mydilation_x() * dx, mydilation_y() * dy);
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


(** Represente la longueur de la matrice en x*)
let mymatrix_dx() : int =
  100;;

(** Represente la longueur de la matrice en y *)
let mymatrix_dy() : int =
  100;;


type t_matrix = t_value matrix;;
type t_snake = t_position list;;
type t_play = {dt : float ref; sn : t_snake ref; mat : t_matrix};;
(** Represente la vitesse initiale du serpent *)
let mydt_init() : float =
  0.1;;
(** Reprensente  l'intervalle entre deux mouvements *)
let mydt_acc() : float =
3.0
;;
(** Reprensente le ratio entre la nouvelle position et la precedente*)
let mydt_ratio() : float =
0.1
;;
(** Represente la longueur du serpent au point initiale *)
let mysnake_length_init() : int =
3
;;
(** Represente les coordonnees de la position initiale du serpent *)
let mysnake_position_init () : t_point =
  {x = 20; y = 20};;

(** Dessine le cadre autour de la matrice de jeu *)
let draw_frame() : unit =
  set_color(Graphics.black);
  for i = 0 to 4
  do
    draw_rect(0 -i+mytranslation_x(),
              0-i+mytranslation_y(),
              ((mytranslation_x()-1)+i/mydilation_x())*mydilation_x(),
              ((mytranslation_y()-1)+i/mydilation_y())*mydilation_y());
    
  done;;

let rec draw_whole_snake_aux(s : t_snake) : t_snake =
  if s = []
  then []
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
  ();
;;

      
(** Initialise le serpent au debut du jeu *)
let init_snake() : t_snake =
  [{pt = mysnake_position_init(); dir = LEFT};
    {pt = {x = (mysnake_position_init()).x + 5; y = (mysnake_position_init()).y} ; dir = LEFT};
    {pt = {x = (mysnake_position_init()).x + 10; y = (mysnake_position_init()).y}; dir = LEFT}]
;;
  
(** Initialise la matrice de jeu en EMPTY (en blanc)*)
let init_matrix() : t_matrix =
  mat_make(mymatrix_dx(),mymatrix_dy(),EMPTY);;

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

(** Reetourne la nouvelle position en fonction de la direction donnee en parametre et la valeur de la case dans la matrice de jeu
    3 parametres:
    - pos: la position initiale
    - dir: la direction
    - m: la matrice de jeu *)
let compute_move(pos, dir, m : t_position * t_direction * t_matrix) : t_position * t_value =
  let new_pos : t_position = compute_new_position(pos,dir) in
  (
    if(new_pos.pt.x < 0 || new_pos.pt.x > mymatrix_dx() || new_pos.pt.y < 0 || new_pos.pt.y > mymatrix_dy())
    then (new_pos,FRAME)
    else (new_pos,m.(pos.pt.x).(pos.pt.y))
  )
;;

(** Eneleve la case de la queue du serpent, mets a jour la matrice et efface le graphique.
    1 parametre:
    - pl : represente le plateau de jeu *)
let remove_snake_tail(pl : t_play) : unit =
  let pos_x : int = (lst(!(pl.sn))).pt.x and pos_y : int = (lst(!(pl.sn))).pt.y and mat : t_value matrix =  pl.mat in
  (
    mat.(pos_x).(pos_y) <- EMPTY;
    set_color(color_of_value(EMPTY));
    myplot(pos_x,pos_y);
    pl.sn := rem_lst(pl.sn.contents);
  )
;;

(** Ajoute la tete du serpent a la position donnee et l'affiche sur le graphique.
    2 parametres:
    - pl: represente le plateau de jeu
    - newpos: nouvelle position de la tete *)

let add_snake_newhead(pl, newpos : t_play * t_position) : unit =
    pl.mat.(newpos.pt.x).(newpos.pt.y) <- SNAKE;
    set_color(color_of_value(SNAKE));
    myplot(newpos.pt.x,newpos.pt.y);
    pl.sn := add_fst(!(pl.sn),newpos);
;;

(** Simule un déplacement du serpent *)

let move_snake ( pl , newpos : t_play * t_position ) : unit =
  compute_move( (fst(!(pl.sn))) , (fst(!(pl.sn))).dir , pl.mat);
  (
    for i =  0 to len(snake) 
    do         
      pl.sn := (nth(!(pl.sn),i) ;
    done;
  )
    remove_snake_tail(pl);
  add_snake_newhead(pl,newpos);
;;


let new_step (pl : t_play ) : bool =


;;




  
     
  
  

  
  
