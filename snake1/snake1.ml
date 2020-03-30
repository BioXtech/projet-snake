
(*#use "CPinter_sn.ml";;
#use "CPsnake_misc.ml";;*)


open CPutil_sn;;
open_graph(700,700);;

(** <b>Ce fichier est la premi�re version du jeu snake. Il est compos� des diff�rentes fonctions et type utilis�s pour r�aliser le jeu.</b>
    @version 1.0
 *)

(** Fonction qui donne la valeur constante de d�calage sur x.
    @return retourne la valeur de d�calage sur x de la matrice de jeu.
    @author Duc
    @author Guillaume.
    @since 1.0

*)
let mytranslation_x() : int =
  100
;;

(** Fonction qui donne la valeur constante de d�calage sur y.
    @return retourne la valeur de d�calage sur y de la matrice de jeu.
    @author Duc
    @author Guillaume.
    @since 1.0 

*)
let mytranslation_y() : int =
  100
;;

(** Fonction qui donne le coefficiant constant de dilatation sur x.
    @return la valeur du coefficiant de dilatation sur x de la matrice de jeu.
    @author Duc
    @author Guillaume.
    @since 1.0
*)
let mydilation_x() : int =
  5
;;

(** Fonction qui donne le coefficiant constant de dilatation sur y. 
    @return la valeur du coefficiant de dilatation sur y de la matrice de jeu.
    @author Duc
    @author Guillaume.
    @since 1.0
*)
let mydilation_y() : int =
  5
;;

(** Fonction qui calcule la coordonnee x d'un pixel avec son d�calage et son coefficiant de dilatation.
    @param x coordonn�e du pixel sur x.
    @return coordonn�e du pixel sur x.
    @author Duc.
    @since 1.0
*)
let mygraphic_x(x : int) : int =
  mytranslation_x() + x * mydilation_x()
;;

(** Fonction qui calcule la coordonnee y d'un pixel avec son d�calage et son coefficiant de dilatation.
    @param y coordonn�e du pixel sur y.
    @return coordonn�e du pixel sur y.
    @author Duc.
    @since 1.0
*)
let mygraphic_y(y : int) : int =
  mytranslation_y() + y * mydilation_y()
;;

(** Affiche dans la fenetre d'affichage un rectangle correspondant � un pixel donn� en param�tre.
    @param x coordonnee sur x du pixel.
    @param y coordonnee sur y du pixel.
    @author Guillaume.
    @since 1.0
*)
let myplot(x,y : int * int) : unit =
  fill_rect(mygraphic_x(x),mygraphic_y(y),mydilation_x(),mydilation_y());
;;


(** Trace dans la fenetre d'affichage le rectangle vide.
    @param px coordonnee x du point en bas a gauche.
    @param py coordonnee y du point en bas a gauche.
    @param dx longueur du c�t� sur x.
    @param dy longueur du c�t� sur y.
    @author Guillaume.
    @since 1.0
 *)
let myfill_rect(px, py, dx, dy : int * int * int * int) : unit =
  fill_rect(mygraphic_x(px), mygraphic_y(py),mydilation_x() * dx,mydilation_y() * dy);
;;

(* Type et fonction de base *)

(** Represente les coordonn�es d'un point.
    @since 1.0
 *)
type t_point = {
    x : int; (** Coordonn�e sur l'abscisse du point. *) 
    y : int  (** Coordonn�e sur l'ordonn�e du point. *)
};;


(** Represente la direction que prend le serpent.
    @since 1.0 
*)
type t_direction = UP | DOWN | RIGHT | LEFT;;

(** Represente la position du serpent et la direction.
    @since 1.0
*)
type t_position = {
    pt : t_point; (** Represente les coordonn�es de la position. *)
    dir : t_direction (** Represente la direction dans laquelle la case du serpent va. *)
};;

(** Reprensente la valeur d'une case 
    @since 1.0
*)
type t_value = EMPTY | SNAKE | FRAME | PROBLEM ;;

(** En fonction de la valeur de la case, donne la couleur correspondante.
    @param x type de la case.
    @return la couleur de la case.
    @author Duc.
    @since 1.0
 *)
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


(** Repr�sente la longueur de la matrice en x.
    @return la valeur de la longueur de la matrice en x.
    @author Duc
    @author Guillaume.
    @since 1.0
*)
let mymatrix_dx() : int =
  100;;

(** Repr�sente la longueur de la matrice en y.
    @return la valeur de la longueur de la matrice en y.
    @author Duc
    @author Guillaume.
    @since 1.0
*)
let mymatrix_dy() : int =
  100;;

(** Repr�sente la matrice de jeu. 
    @since 1.0
*)
type t_matrix = t_value matrix;;

(** Repr�sente le serpent qui est une liste de postition. 
    @since 1.0
*)
type t_snake = t_position list;;

(** Repr�sente le jeu avec sa vitesse, le serpent et la matrice de jeu. 
    @since 1.0
*)
type t_play = {
    dt : float ref;(** Vitesse du serpent. *)
    sn : t_snake ref;(** Serpent *)
    mat : t_matrix(** Matrice de jeu contenant les cases *)
  };;

(** Fonction qui donne la vitesse initiale du serpent.
    @return la valeur de la vitesse initiale.
    @author Duc
    @author Guillaume
    @since 1.0
*)
let mydt_init() : float =
  0.1;;

(** Fonction qui donne l'intervalle de temps entre deux modifications de la vitesse.
    @return la valeur de l'intervalle.
    @author Duc
    @author Guillaume.
    @since 1.0
 *)
let mydt_acc() : float =
  3.0
;;

(** Fonction qui donne le ratio entre l'ancienne et la nouvelle vitessse.
    @return le ratio de vitesse.
    @author Duc
    @author Guillaume.
    @since 1.0
*)
let mydt_ratio() : float =
  0.1
;;

(** Fonction qui donne la longueur initiale du serpent.
    @return la longueur initiale du serpent.
    @author Duc
    @author Guillaume.
    @since 1.0
 *)
let mysnake_length_init() : int =
3
;;

(** Fonction qui donne les coordonn�es de la position initiale du serpent.
    @return les coordonn�es intiales du serpent.
    @author Duc
    @author Guillaume.
    @since 1.0
 *)
let mysnake_position_init () : t_point =
  {x = 20; y = 20}
;;

(** Fonction qui dessine le cadre autour de la matrice de jeu.
    @author Duc
    @author Guillaume.
    @since 1.0
 *)
let draw_frame() : unit =
  myfill_rect(-1,-1,1 ,mymatrix_dy());
  myfill_rect(-1, -1, mymatrix_dx(), 1);
  myfill_rect(-1, mymatrix_dy()-1, mymatrix_dx() + 1 ,1);
  myfill_rect(mymatrix_dx()-1, -1, 1 ,mymatrix_dy() + 1);
;;

(** Fonction auxiliaire � draw_whole_snake() pour dessiner le serpent.
    @param s liste de positions repr�sentant le serpent.
    @author Guillaume.
    @since 1.0
 *)
let rec draw_whole_snake_aux(s : t_snake) : unit =
  if s = []
  then ()
  else
    (
      myplot((fst(s)).pt.x,(fst(s)).pt.y);
      draw_whole_snake_aux(rem_fst(s));
    )
;;

(** @author Guillaume.
    @since 1.0
    Fonction qui dessine la totalit� du serpent.
    @param s liste de positions repr�sentant le serpent.
 *)
let draw_whole_snake(s : t_snake) : unit =
  set_color(Graphics.green);
  draw_whole_snake_aux(s);
;;

(** Fonction auxilliaire de init_snake().
    @param i longueur initiale du serpent.
    @return le serpent.
    @author Guillaume.
    @since 1.0
*)
let rec init_snake_aux(i : int) : t_snake =
  if i <= 1
  then [{pt = {x = (mysnake_position_init()).x;
               y = (mysnake_position_init()).y}; dir = LEFT}]
  else add_lst(init_snake_aux(i - 1),{pt = {x = (mysnake_position_init()).x -1 + i;
                      y = (mysnake_position_init()).y} ; dir = LEFT})
;;
(** Fonction qui initialise le serpent au d�but du jeu.
    @return le serpent.
    @author Guillaume.
    @since 1.0
 *)
let init_snake() : t_snake =
  init_snake_aux(mysnake_length_init())
;;

(** Fonction qui initialise la matrice de jeu en EMPTY (en blanc).
    @return la matrice de jeu.
    @author Duc.
    @since 1.0
*)
let init_matrix() : t_matrix =
  mat_make(mymatrix_dx(),mymatrix_dy(),EMPTY)
;;

(*voir pour l'efficacite*)
(** Fonction qui ins�re les positions du serpent dans la matrice de jeu globale.
    @return le serpent et la matrice de jeu.
    @author Guillaume.
    @since 1.0
 *)
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

(** Fonction qui initialise le plateau de jeu avec le cadre et le serpent
    @return le plateau de jeu.
    @author Duc
    @author Guillaume.
    @since 1.0
*)
let init_play() : t_play =
  let (snake,matrix) : t_snake * t_matrix = init_snake_matrix() in
  draw_frame();
  draw_whole_snake(init_snake());
  ({dt = {contents = mydt_init()}; sn = {contents = snake}; mat = matrix})
;;

(** Fonction qui met � jour la position d'une case du serpent en fonction de la direction en entr�e.
    @param pos position de la case du serpent.
    @param d direction de la case.
    @return la position mise � jour de la case en entr�e.
    @author Guillaume.
    @since 1.0
*)
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

(** Fonction qui met � jour la position d'une case en fonction de la direction donn�e et la valeur de la case dans la matrice de jeu.
    @param pos la position initiale de la case.
    @param dir la direction de la case.
    @param m la matrice de jeu. 
    @return la case mise � jour et la matrice mise � jour.
    @author Guillaume.
    @since 1.0
*)
let compute_move(pos, dir, m : t_position * t_direction * t_matrix) : t_position * t_value =
  let new_pos : t_position = compute_new_position(pos,dir) in
  (
    if(new_pos.pt.x < 0 || new_pos.pt.x > mymatrix_dx() || new_pos.pt.y < 0 || new_pos.pt.y > mymatrix_dy())
    then (new_pos,FRAME)
    else (new_pos,m.(pos.pt.x).(pos.pt.y))
  )
;;

(** Fonction qui enel�ve la case de la queue du serpent, met � jour la matrice de jeu et efface la case du graphique.
    @param pl repr�sente le plateau de jeu. 
    @author Guillaume.
    @since 1.0
*)
let remove_snake_tail(pl : t_play) : unit =
  let pos_x : int = (lst(!(pl.sn))).pt.x and pos_y : int = (lst(!(pl.sn))).pt.y and mat : t_value matrix =  pl.mat in
  (
    mat.(pos_x).(pos_y) <- EMPTY;
    set_color(color_of_value(EMPTY));
    myplot(pos_x,pos_y);
    pl.sn := rem_lst(pl.sn.contents);
  )
;;

(** Fonction qui ajoute la t�te du serpent � la position donn�e et l'affiche sur le graphique.
    @param pl repr�sente le plateau de jeu.
    @param newpos nouvelle position de la t�te. 
    @author Guillaume.
    @since 1.0
*)
let add_snake_newhead(pl, newpos : t_play * t_position) : unit =
    pl.mat.(newpos.pt.x).(newpos.pt.y) <- SNAKE;
    set_color(color_of_value(SNAKE));
    myplot(newpos.pt.x,newpos.pt.y);
    pl.sn := add_fst(!(pl.sn),newpos);
;;
(** Fonction auxiliaire qui met � jour les diff�rentes direction de chaque case du snake.
    @param s le serpent.
    @return le serpent avec les directions mises � jour.
    @author Guillaume.
    @since 1.0
 *)
let update_direction(s : t_snake) : t_snake =
  let new_snake : t_snake ref = ref [] and imax : int = len(s) - 1 in
  (
    for i = 1 to imax
    do
      new_snake := add_lst(!new_snake,nth(s,i-1));
    done;
    new_snake := add_lst(!new_snake,(nth(s,len(s)-1)));
    !new_snake;
  )
;;

(** Fonction qui simule le d�placement du serpent dans le plateau de jeu.
    @param pl repr�sente le plateau du jeu.
    @param newpos repr�sente la nouvelle position du serpent.
    @author Duc
    @author Guillaume
    @since 1.0
 *)
let move_snake ( pl , newpos : t_play * t_position ) : unit =
  pl.sn := update_direction(!(pl.sn));
  add_snake_newhead(pl,newpos);
  remove_snake_tail(pl);
;;

let analyze(pos : t_position) : t_direction =
  let (i, j) : int * int = mouse_pos() in
  let (k, l) : int * int = (mygraphic_x(pos.pt.x), mygraphic_y(pos.pt.y)) in
  let v : t_direction =
    if abs(i - k) < abs(j - l)
    then 
      if j > l 
      then UP
      else DOWN
    else
      if i < k
      then LEFT
      else RIGHT
  in
    if (pos.dir = UP && v = DOWN) || (pos.dir = DOWN && v = UP)
    then if i < k then LEFT else RIGHT
    else 
      if (pos.dir = LEFT && v = RIGHT) || (pos.dir = RIGHT && v = LEFT)
      then if j > l then UP else DOWN
      else v
;;

(** Fonction qui effectue une nouvelle �tape de jeu
    @param pl le plateau de jeu
    @author Guillaume.
    @since 1.0
 *)
let new_step(pl : t_play) : bool =
  let new_dir : t_direction = analyze(fst(!(pl.sn))) in
  let new_pos : t_position = compute_new_position(fst(!(pl.sn)),new_dir) in
  (
    if(not(new_pos.pt.x < 0 || new_pos.pt.x > mymatrix_dx()-1 || new_pos.pt.y < 0 || new_pos.pt.x > mymatrix_dy()-1 || pl.mat.(new_pos.pt.x).(new_pos.pt.y) = SNAKE))
    then (
      move_snake(pl,new_pos);
      false;
    )
    else (
      draw_string("Vous avez perdu !");
      true;
    )
  )
;;

(** 
<h1>Explication de la fonction <code>handle_t_acc</code>.</h1>
Si la diff�rence entre le temps actuel et la derni�re fois que la valeur de la vitesse � �t� chang�e est plus grande que l'intervalle de temps minimum n�cessaire pour changer la valeur de la vitesse alors la vitesse prend la valeur de la vitesse actuelle multipli�e par le ratio d'acc�l�ration.
La valeur de <code>t_acc</code> est remise "� z�ro" ou "� l'heure" car on a chang� la valeur de la vitesse.

<h1>Explication de la fonction <code>simulation</code>.</h1>
 Il y a d'abord la d�claration des variables :
 <ul>
   <li><code>pl</code> : repr�sente le plateau de jeu.</li>
   <li><code>t</code> :  repr�sente la valeur du temps au d�but d'un tour de boucle.</li>
   <li><code>newt</code> : repr�sente le temps actuel.</li>
   <li><code>t_acc</code> : repr�sente le temps au moment de la derni�re modification de la vitesse.</li>
   <li><code>thend</code> : booleen representant la fin de partie.</li>
 </ul>
 Il y a une boucle principale qui permet de faire tourner le jeu avec tant que <code>thend</code> est faux.
 Dans cette boucle il y a :
 <ol>
   <li>La valeur de <code>newt</code> est mise � jour.</li>
   <li>La boucle while suivante permet de mettre halte au programme tant que il ne s'est pas pass� assez de temps pour simuler la dur�e entre les intervalles de position avec une vitesse.</li>
   <li>A la sortie de la boucle, il s'est pass� assez de temps pour faire avancer le serpent et �ventuellement augmenter la vitesse avec <code>handle_t_acc</code>.</li>
   <li><code>thend</code> prend la valeur retourn�e par la fonction <code>new_step</code> qui simule une �tape de jeu</li>
 </ol>
*)
