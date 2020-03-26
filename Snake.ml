(*#use "CPinter_sn.ml";;*)
open CPutil_sn;;
open_graph(700,700);;
(** Ce fichier est la premi�re version du jeu snake. Il est compos� des diff�rentes fonctions et type utilis�s pour r�aliser le jeu.
    @version 1.0
 *)

(** @author Duc
    @author Guillaume
    ---
    Fonction qui donne la valeur constante de d�calage sur x.
    @return retourne la valeur de d�calage sur x de la matrice de jeu.
**)
let mytranslation_x() : int =
  100
;;

(** @author Duc
    @author Guillaume
    Fonction qui donne la valeur constante de d�calage sur y.
    @return retourne la valeur de d�calage sur y de la matrice de jeu.
*)
let mytranslation_y() : int =
  100
;;

(** @author Duc
    @author Guillaume
    Fonction qui donne le coefficiant constant de dilatation sur x.
    @return la valeur du coefficiant de dilatation sur x de la matrice de jeu.
*)
let mydilation_x() : int =
  5
;;

(** @author Duc
    @author Guillaume
    Fonction qui donne le coefficiant constant de dilatation sur y. 
    @return la valeur du coefficiant de dilatation sur y de la matrice de jeu.
*)
let mydilation_y() : int =
  5
;;

(** @author Duc
    Fonction qui calcule la coordonnee x d'un pixel avec son d�calage et son coefficiant de dilatation.
    @param x coordonn�e du pixel sur x.
    @return coordonn�e du pixel sur x.
*)
let mygraphic_x(x : int) : int =
  mytranslation_x() + x * mydilation_x()
;;

(** @author Duc
    Fonction qui calcule la coordonnee y d'un pixel avec son d�calage et son coefficiant de dilatation.
    @param y coordonn�e du pixel sur y.
    @return coordonn�e du pixel sur y.
*)
let mygraphic_y(y : int) : int =
  mytranslation_y() + y * mydilation_y()
;;

(** @author Guillaume
    Affiche dans la fenetre d'affichage un rectangle correspondant � un pixel donn� en param�tre.
    @param x coordonnee sur x du pixel.
    @param y coordonnee sur y du pixel.
*)
let myplot(x,y : int * int) : unit =
  fill_rect(mygraphic_x(x),mygraphic_y(y),mydilation_x(),mydilation_y());
;;


(** @author Guillaume
    Trace dans la fenetre d'affichage le rectangle vide.
    @param px coordonnee x du point en bas a gauche.
    @param py coordonnee y du point en bas a gauche.
    @param dx longueur du c�t� sur x.
    @param dy longueur du c�t� sur y.
 *)
let myfill_rect(px, py, dx, dy : int * int * int * int) : unit =
  fill_rect(mygraphic_x(px), mygraphic_y(py),mydilation_x() * dx,mydilation_y() * dy);
;;

(* Type et fonction de base *)

(** Represente les coordonn�es d'un point. *)
type t_point = {
    x : int; (** Coordonn�e sur l'abscisse du point. *) 
    y : int  (** Coordonn�e sur l'ordonn�e du point. *)
};;


(** Represente la direction que prend le serpent. *)
type t_direction = UP | DOWN | RIGHT | LEFT;;

(** Represente la position du serpent et la direction.*)
type t_position = {
    pt : t_point; (** Represente les coordonn�es de la position. *)
    dir : t_direction (** Represente la direction dans laquelle la case du serpent va. *)
};;

(** Reprensente la valeur d'une case *)
type t_value = EMPTY | SNAKE | FRAME | PROBLEM ;;

(** @author Duc
    En fonction de la valeur de la case, donne la couleur correspondante.
    @param x type de la case.
    @return la couleur de la case.
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


(** @author Duc
    @author Guillaume
    Repr�sente la longueur de la matrice en x.
    @return la valeur de la longueur de la matrice en x.
*)
let mymatrix_dx() : int =
  100;;

(** @author Duc
    @author Guillaume
    Repr�sente la longueur de la matrice en y.
    @return la valeur de la longueur de la matrice en y.
*)
let mymatrix_dy() : int =
  100;;

(** Repr�sente la matrice de jeu. *)
type t_matrix = t_value matrix;;

(** Repr�sente le serpent qui est une liste de postition. *)
type t_snake = t_position list;;

(** Repr�sente le jeu avec sa vitesse, le serpent et la matrice de jeu. *)
type t_play = {
    dt : float ref;(** Vitesse du serpent. *)
    sn : t_snake ref;(** Serpent *)
    mat : t_matrix(** Matrice de jeu contenant les cases *)
  };;

(** @author Duc
    @author Guillaume
    Fonction qui donne la vitesse initiale du serpent.
    @return la valeur de la vitesse initiale.
*)
let mydt_init() : float =
  0.1;;

(** @author Duc
    @author Guillaume
    Fonction qui donne l'intervalle de temps entre deux modifications de la vitesse.
    @return la valeur de l'intervalle.
 *)
let mydt_acc() : float =
  3.0
;;

(** @author Duc
    @author Guillaume
    Fonction qui donne le ratio entre l'ancienne et la nouvelle vitessse.
    @return le ratio de vitesse.
*)
let mydt_ratio() : float =
  0.1
;;

(** @author Duc
    @author Guillaume
    Fonction qui donne la longueur initiale du serpent.
    @return la longueur initiale du serpent.
 *)
let mysnake_length_init() : int =
3
;;

(** @author Duc
    @author Guillaume
    Fonction qui donne les coordonn�es de la position initiale du serpent.
    @return les coordonn�es intiales du serpent.
 *)
let mysnake_position_init () : t_point =
  {x = 20; y = 20}
;;

(** @author Duc
    @author Guillaume
    Fonction qui dessine le cadre autour de la matrice de jeu. *)
let draw_frame() : unit =
  myfill_rect(0, 0, 1 ,mymatrix_dy());
  myfill_rect(0, 0, mymatrix_dx(), 1);
  myfill_rect(0, mymatrix_dy(), mymatrix_dx() + 1 ,1);
  myfill_rect(mymatrix_dx(), 0, 1 ,mymatrix_dy() + 1);
;;

(** @author Guillaume
    Fonction auxiliaire � <code>draw_whole_snake()</code> pour dessiner le serpent.
    @param s liste de positions repr�sentant le serpent.
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

(** @author Guillaume
    Fonction qui dessine la totalit� du serpent.
    @param s liste de positions repr�sentant le serpent.
 *)
let draw_whole_snake(s : t_snake) : unit =
  set_color(Graphics.green);
  draw_whole_snake_aux(s);
;;

(** @author Guillaume
    Fonction auxilliaire de <code>init_snake()</code>.
    @param i longueur initiale du serpent.
    @return le serpent.
*)
let rec init_snake_aux(i : int) : t_snake =
  if i = 1
  then [{pt = {x = (mysnake_position_init()).x;
               y = (mysnake_position_init()).y}; dir = LEFT}]
  else add_lst(init_snake_aux(i - 1),{pt = {x = (mysnake_position_init()).x -1 + i;
                      y = (mysnake_position_init()).y} ; dir = LEFT})

(** @author Guillaume
    Fonction qui initialise le serpent au d�but du jeu.
    @return le serpent.
 *)
let init_snake() : t_snake =
  init_snake_aux(mysnake_length_init())
;;

(** @author Duc
    Fonction qui initialise la matrice de jeu en EMPTY (en blanc).
    @return la matrice de jeu.
*)
let init_matrix() : t_matrix =
  mat_make(mymatrix_dx(),mymatrix_dy(),EMPTY)
;;

(*voir pour l'efficacite*)
(** @author Guillaume
    Fonction qui ins�re les positions du serpent dans la matrice de jeu globale.
    @return le serpent et la matrice de jeu.
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

(** @author Duc
    @author Guillaume
    Fonction qui initialise le plateau de jeu avec le cadre et le serpent
    @return le plateau de jeu.
*)
let init_play() : t_play =
  draw_frame();
  draw_whole_snake(init_snake());
  {dt = {contents = mydt_init()}; sn = {contents = init_snake()}; mat = init_matrix()}
;;

(** @author Guillaume
    Fonction qui met � jour la position d'une case du serpent en fonction de la direction en entr�e.
    @param pos position de la case du serpent.
    @param d direction de la case.
    @return la position mise � jour de la case en entr�e.
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

(** @author Guillaume
    Fonction qui met � jour la position d'une case en fonction de la direction donn�e et la valeur de la case dans la matrice de jeu.
    @param pos la position initiale de la case.
    @param dir la direction de la case.
    @param m la matrice de jeu. 
    @return la case mise � jour et la matrice mise � jour.
*)
let compute_move(pos, dir, m : t_position * t_direction * t_matrix) : t_position * t_value =
  let new_pos : t_position = compute_new_position(pos,dir) in
  (
    if(new_pos.pt.x < 0 || new_pos.pt.x > mymatrix_dx() || new_pos.pt.y < 0 || new_pos.pt.y > mymatrix_dy())
    then (new_pos,FRAME)
    else (new_pos,m.(pos.pt.x).(pos.pt.y))
  )
;;

(** @author Guillaume
    Fonction qui enel�ve la case de la queue du serpent, met � jour la matrice de jeu et efface la case du graphique.
    @param pl repr�sente le plateau de jeu. 
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

(** @author Guillaume
    Fonction qui ajoute la t�te du serpent � la position donn�e et l'affiche sur le graphique.
    @param pl repr�sente le plateau de jeu.
    @param newpos nouvelle position de la t�te. 
*)
let add_snake_newhead(pl, newpos : t_play * t_position) : unit =
    pl.mat.(newpos.pt.x).(newpos.pt.y) <- SNAKE;
    set_color(color_of_value(SNAKE));
    myplot(newpos.pt.x,newpos.pt.y);
    pl.sn := add_fst(!(pl.sn),newpos);
;;

(** 

<h1>Explication de la fonction <code>handle_t_acc</code>.</h1>
<center>Si la diff�rence entre le temps actuel et la derni�re fois que la valeur de la vitesse � �t� chang�e est plus grande que l'intervalle de temps minimum n�cessaire pour changer la valeur de la vitesse alors la vitesse prend la valeur de la vitesse actuelle multipli�e par le ratio d'acc�l�ration.</center>
<h5>La valeur de <code>t_acc</code> est remise "� z�ro" ou "� l'heure" car on a chang� la valeur de la vitesse.</h5>

<h1>Explication de la fonction <code>simulation</code>.</h1>
 <h5>Il y a d'abord la d�claration des variables :</h5>
 <ul>
   <li><code>pl</code> : repr�sente le plateau de jeu.</li>
   <li><code>t</code> :  repr�sente la valeur du temps au d�but d'un tour de boucle.</li>
   <li><code>newt</code> : repr�sente le temps actuel.</li>
   <li><code>t_acc</code> : repr�sente le temps au moment de la derni�re modification de la vitesse.</li>
   <li><code>thend</code> : booleen representant la fin de partie.</li>
 </ul>
 <h5>Il y a une boucle principale qui permet de faire tourner le jeu avec tant que <code>thend</code> est faux.</h5>
 <h5>Dans cette boucle il y a :</h5>
 <ol>
   <li>La valeur de <code>newt</code> est mise � jour.</li>
   <li>La boucle while suivante permet de mettre halte au programme tant que il ne s'est pas pass� assez de temps pour simuler la dur�e entre les intervalles de position avec une vitesse.</li>
   <li>A la sortie de la boucle, il s'est pass� assez de temps pour faire avancer le serpent et �ventuellement augmenter la vitesse avec <code>handle_t_acc</code>.</li>
   <li><code>thend</code> prend la valeur retourn�e par la fonction <code>new_step</code> qui simule une �tape de jeu</li>
 </ol>
*)
