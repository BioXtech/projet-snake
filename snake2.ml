(** Jeu de serpent 

Le descriptif complet de cette version du jeu figure sur UPdago, cours de Complement de Programmation, section "Jeu de serpent", fichier "snake_partie_1.pdf" ;

3 elements sont essentiels : le serpent, son environnement (la matrice de jeu), la fenetre d'affichage. La partie de la fenetre d'affichage correspondant a la matrice de jeu est appelee la zone de jeu. *)

(* --------------------------*)
(* --------------------------*)
(*   NB. Ceci est un corrige *)
(*   Ce n'est donc qu'une    *)
(*   version possible !      *)
(*   D'autres versions       *)
(*   existent, qui peuvent   *)
(*   aussi repondre a ce qui *)
(*   etait demande           *)
(* --------------------------*)
(* --------------------------*)


(*open CPutil_sn ;;*)
#use "CPinter_sn.ml";;
open_graph(700,700);;

(* --------------------------*)
(*   fonctions graphiques    *)
(* --------------------------*)
(** {2 Fonctions graphiques} *)

(** {3 Parametrage} *)

(** donne le decalage en x de la zone de jeu par rapport a l'origine de la fenetre d'affichage *)
let mytranslation_x() :int = 
  100 
;;

(** donne le decalage en y de la zone de jeu par rapport a l'origine de la fenetre d'affichage *)
let mytranslation_y() :int = 
  100 
;;

(** donne le nombre de pixels de la zone de jeu (en x) correspondant a un point de la matrice de jeu *) 
(*(** donne le rapport entre la taille en x de la zone de jeu par rapport a la matrice de jeu *)*)
let mydilation_x() : int = 
  5
;;

(** donne le nombre de pixels de la zone de jeu (en y) correspondant a un point de la matrice de jeu *) 
(*(** donne le rapport entre la taille en y de la zone de jeu par rapport a la matrice de jeu *)*)
let mydilation_y() : int = 
  5 
;;

(** {3 Transformation geometrique} *)

(** calcule l'abscisse du pixel en bas a gauche du rectangle de pixels de la zone de jeu qui correspond a un point dans la matrice de jeu, a partir de l'abscisse de ce point *)
let mygraphic_x(x : int) : int =
  (x * mydilation_x()) + mytranslation_x()
;;

(** calcule l'ordonnee du pixel en bas a gauche du rectangle de pixels de la zone de jeu qui correspond a un point dans la matrice de jeu, a partir de l'ordonnee de ce point *)
let mygraphic_y(y : int) : int = 
  (y * mydilation_y()) + mytranslation_y()
;;

(** {3 Affichage} *)
(** affiche le rectangle de pixels de la zone de jeu qui correspond a un point dans la matrice de jeu, a partir des coordonnees de ce point *)
let myplot(x, y : int * int) : unit =
    fill_rect(mygraphic_x(x), mygraphic_y(y), mydilation_x(), mydilation_y()) ;
;;

(** affiche le rectangle de pixels de la zone de jeu qui correspond a un rectangle dans la  matrice de jeu.

Les deux premiers parametres sont les coordonnees du point en bas a gauche du rectangle de la matrice de jeu ;

les deux derniers parametres sont la largeur et la hauteur du rectangle de la matrice de jeu.
*)
let myfill_rect(x, y, dx, dy : int * int * int * int) : unit =
    fill_rect(mygraphic_x(x), mygraphic_y(y), dx * mydilation_x(), dy * mydilation_y())
;;


(* ------------------------------------- *)
(* les types pour les elements du jeu :  *)
(* serpent, matrice de jeu               *)
(* ------------------------------------- *)
(** {2 Les types pour les elements du jeu} *)

(* --------------------------*)
(*       le serpent          *)
(* --------------------------*)
(** {3 Le serpent} *)

(* les directions de deplacement  *)

(** valeurs possibles caracterisant le deplacement de la tete du serpent *)
type t_direction = LEFT | RIGHT | UP | DOWN ;;

(** pour la representation de points 2D a coordonnees entieres *)
type t_point = {x : int ; y : int} ;;

(** une position est definie par un point et une direction *)
type t_position = {pt : t_point ; dir : t_direction} ;;


(** un serpent est une liste de positions *)
type t_snake = t_position list ;;

(* --------------------------*)
(*       la matrice          *)
(* --------------------------*)
(** {3 La matrice} *)

(** valeurs possibles pour les elements de la matrice de jeu ; dans cette version du jeu, les valeurs FRAME et PROBLEM, bien qu'utilisees dans le jeu, ne sont pas utilisees comme valeurs d'elements de la matrice de jeu *)
type t_value = EMPTY | SNAKE | FRAME | PROBLEM | BONUS ;;

(** pour la representation d'une matrice de jeu *)
type t_matrix = t_value matrix;;

(* ------------------------------ *)
(* container                      *)
(* ------------------------------ *)
(** {3 Le container} *)

(** container rassemblant les caracteristiques d'un jeu *)
type t_play = { dt : float ref ; sn : t_snake ref ; mat : t_matrix ; score : int ref};;

(* ------------------------------ *)
(* conversion en couleurs des     *)
(* valeurs possibles d'un element *)
(* de la matrice                  *)
(* ------------------------------ *)
(** {3 Fonction utilitaire : conversion valeur de matrice <-> couleur} *)

(** retourne la couleur associee a une valeur possible d'un element de la matrice *)
let color_of_value(x : t_value) : t_color =
  if x = EMPTY
  then white
  else
    if x = SNAKE
    then green
    else
      if x = FRAME
      then black
      else
        if x = BONUS
        then magenta
        else red
;;

(** {2 Parametrage des elements du jeu} *)
(* --------------------------*)
(*       la matrice          *)
(* --------------------------*)
(** {3 Matrice} *)

(** donne la taille en x de la matrice de jeu *)
let mymatrix_dx() : int = 
  100 
;;

(** donne la taille en y de la matrice de jeu *)
let mymatrix_dy() : int = 
  100 
;;


(* --------------------------*)
(* initialisation parametres *)
(* --------------------------*)
(** {3 Temps et vitesse} *)
(** donne l'unite de temps au depart du jeu *)
let mydt_init() : float =
  0.4
;;

(** donne l'intervalle de temps entre deux accelerations *)
let mydt_acc() : float =
  5.0
;;

(** donne le ratio entre les unites de temps apres et avant acceleration *)
let mydt_ratio() : float =
  0.8
;;

(** {3 Serpent} *)
(** donne la longueur du serpent au depart du jeu *)
let mysnake_length_init() : int =
  15
;;

(** donne la position de la tete du serpent au depart du jeu *)
let mysnake_position_init() : t_point =
  {x = mymatrix_dx() / 2 ; y = mymatrix_dy() / 2}
;;



(* --------------------------*)
(* initialisation du jeu     *)
(* --------------------------*)
(** {2 Initialisation du jeu} *)

(* fonctions de dessin pour l'initialisation *)
(** {3 Fonctions de dessin pour l'initialisation} *)

(** dessine le cadre entourant la zone de jeu *)
let draw_frame() : unit =
  (
  set_color(color_of_value(FRAME)) ;
  myfill_rect(-1, -1, mymatrix_dx() + 2, 1) ;
  myfill_rect(-1, mymatrix_dy(), mymatrix_dx() + 2, 1) ;
  myfill_rect(-1, 0, 1, mymatrix_dy()) ;
  myfill_rect(mymatrix_dx(), 0, 1, mymatrix_dy()) ;
  )
;;

(** devrait etre une fonction locale a draw_whole_snake ; laissee en fonction globale afin de pouvoir tracer son execution dans l'interpreteur avec la commande #trace *)
let rec draw_whole_snake_aux(s : t_snake) : unit = 
  if s = []
  then ()
  else
    let (f, r) : t_position * t_snake = (fst(s), rem_fst(s)) in
      (
      myplot(f.pt.x, f.pt.y) ;
      draw_whole_snake_aux(r) ;
      )
;;

(** dessine le serpent a l'initialisation, cad l'ensemble de ses positions *)
let draw_whole_snake(s : t_snake) : unit = 
  set_color(color_of_value(SNAKE)) ;
  draw_whole_snake_aux(s) ;
;;
    
(** {3 Initialisation du serpent, de la matrice de jeu, du container et de l'affichage} *)
(** devrait etre une fonction locale a init_snake ; laissee en fonction globale afin de pouvoir tracer son execution dans l'interpreteur avec la commande #trace  *)
let rec init_snake_aux(nb, p : int * t_point) : t_snake = 
  if nb = 0
  then []
  else 
    let pos : t_position = {pt = p ; dir = LEFT} in
      add_fst(init_snake_aux(nb - 1, {x = p.x + 1 ; y = p.y}), pos)
;;

(** calcule le serpent au depart du jeu *)
let rec init_snake() : t_snake = 
  init_snake_aux(mysnake_length_init(), mysnake_position_init())
;;

(** initialise une matrice de jeu vide *)
let init_matrix() : t_matrix =
  mat_make(mymatrix_dx(), mymatrix_dy(), EMPTY)
;;


(** initialise le serpent et la matrice de jeu (le serpent est insere dans la matrice de jeu) *)
let init_snake_matrix() : t_snake * t_matrix =
  let s : t_snake = init_snake() in
  let m : t_matrix = init_matrix() in
  let indmax : int = mysnake_length_init() - 1 and 
             p : t_point ref = ref {x = 0 ; y = 0} in
    (
    for i = 0 to indmax 
    do
      p := (nth(s, i)).pt ;
      m.((!p).x).((!p).y) <- SNAKE ;
    done ;
    (s, m) ;
    )
;;

(** initialise le jeu *)
let init_play() : t_play =
  let (s, m) : t_snake * t_matrix = init_snake_matrix() in
    (
      clear_graph();
      draw_frame();
      draw_whole_snake(s) ;
      {dt = ref (mydt_init()) ; sn = ref s ; mat = m; score = ref 0} ;
    )
;; 



(* ---------------------------- *)
(* les fonctions necessaires au *)
(* calcul d'une etape           *)
(* ---------------------------- *)
(** {2 Les fonctions necessaires au calcul d'une etape} *)

(**{3 Calcul de la nouvelle position de la tete et de la valeur correspondante} *)

(** calcule la direction de deplacement en fonction des positions relatives de la souris et de la tete du serpent, en evitant au serpent de se tordre le cou (la tete du serpent ne doit pas tourner a 180 degres) *)
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


(** calcule la position ou devrait se trouver la tete du serpent apres deplacement, a partir de son ancienne position et d'une direction de deplacement *)
let compute_new_position(pos, d : t_position * t_direction) : t_position = 
  let nx : int ref = ref (pos.pt.x) and ny : int ref = ref (pos.pt.y) in
    (
    if d = UP
    then 
      ny := !ny + 1
    else
      if d = RIGHT
      then nx := !nx + 1
      else 
        if d = LEFT
        then nx := !nx - 1
        else
          ny := !ny - 1 ;
    {pt = {x = !nx ; y = !ny} ; dir = d} ;
    )
;;


(** calcule la position ou devrait se trouver la tete du serpent apres deplacement, ainsi que la valeur correspondante de la matrice de jeu, a partir de son ancienne position, d'une direction de deplacement et de la matrice de jeu ; retourne la valeur FRAME si la tete du serpent sort de la matrice de jeu *)
let compute_move(pos, dir, m : t_position * t_direction * t_matrix) : t_position * t_value = 
  let newpos : t_position = compute_new_position(pos, dir) in
    if newpos.pt.x < 0 || newpos.pt.x >= mymatrix_dx() || newpos.pt.y < 0 || newpos.pt.y >= mymatrix_dy()
    then (newpos, FRAME)
    else (newpos, m.(newpos.pt.x).(newpos.pt.y))
;;


(** {3 Deplacement du serpent, par suppression de sa queue et ajout de sa "nouvelle" tete} *)
(** supprime la queue du serpent, c'est a dire sa derniere position *)
let remove_snake_tail(pl : t_play) : unit =
  let last_sn : t_position = lst(!(pl.sn)) in
    (
    pl.sn := rem_lst(!(pl.sn)) ;
    pl.mat.(last_sn.pt.x).(last_sn.pt.y) <- EMPTY ;
    set_color(color_of_value(EMPTY)) ;
    myplot(last_sn.pt.x, last_sn.pt.y) ;
    )
;;

(** ajoute sa nouvelle tete au serpent *)
let add_snake_newhead(pl, newpos : t_play * t_position) : unit =
    (
    pl.sn := add_fst(!(pl.sn), newpos) ;
    pl.mat.(newpos.pt.x).(newpos.pt.y) <- SNAKE ;
    set_color(color_of_value(SNAKE)) ;
    myplot(newpos.pt.x, newpos.pt.y)
    )
;;
    

(** deplace le serpent par ajout de sa nouvelle tete et suppression de sa derniere position *)
let move_snake(pl, newpos : t_play * t_position) : unit =
  (
  add_snake_newhead(pl, newpos) ;
  remove_snake_tail(pl) ;
  )
;;

(** Nouvelles fonctions de la deuxième version du jeu du serpent. *)


(** Fonction qui allonge le serpent d'une case à chaque appel.
    @author Guillaume.
    @since 2.0
*)
let snake_grow(pl : t_play) : unit =
  let snake : t_snake = !(pl.sn) in
  let snake_tail : t_position = lst(snake) in
  let x : int ref = ref snake_tail.pt.x and y : int ref = ref snake_tail.pt.y in
  (
    if snake_tail.dir = UP
    then y := !y - 1
    else
      if snake_tail.dir = LEFT
      then x := !x + 1
      else
        if snake_tail.dir = RIGHT
        then x := !x - 1
        else y := !y + 1;
    pl.sn := add_lst(snake,{pt = {x = !x; y = !y}; dir = snake_tail.dir});
  )
;;
   
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

(** Fonction qui sert à générer les coordonnées du bonus sans conflit avec une case existante.
    @param pl le plateau de jeu.
    @return les coodronées du bonus.
    @author Guillaume
    @since 2.0
 *)
let spawn_bonus_coords(pl : t_play) : (int * int) =
  let matrix : t_matrix = pl.mat and rand_x : int ref = ref (rand_int(0,mymatrix_dx())) and rand_y : int ref = ref (rand_int(0,mymatrix_dy())) in
  (
    while (matrix.(!rand_x).(!rand_y) <> EMPTY)
    do
      rand_x := rand_int(0,mymatrix_dx());
      rand_y := rand_int(0,mymatrix_dy());
    done;
    (!rand_x,!rand_y);
  )
;;

(** Fonction qui fait apparaître un bonus sur une case de la matrice.
    @param pl le plateau de jeu.
    @author Guillaume
    @since 2.0
 *)
let spawn_bonus(pl : t_play) : unit =
  let (rand_x, rand_y : int * int) = spawn_bonus_coords(pl) in
  (
    set_color(color_of_value(BONUS));
    myplot(rand_x,rand_y);
    pl.mat.(rand_x).(rand_y) <- BONUS;    
  )
;;
(** Fonction qui remet le serpent à sa taille initiale.
    @param pl le plateau de jeu.
    @author Guillaume
    @since 2.0
 *)
let snake_initial_length(pl : t_play) : unit =
  let new_snake : t_snake ref = ref [] and snake : t_snake = (!(pl.sn)) in
  (
    for i = 0 to mysnake_length_init()
    do
      new_snake := add_fst(!new_snake, nth(snake,i));
    done;
    pl.sn := !new_snake;
  )
;;


(** {3 Calcul d'une etape de jeu} *)
(** deroule une etape de jeu : calcule la nouvelle position de la tete du serpent ainsi que la valeur correspondante, et traite les differents cas : sortie de la zone de jeu, collision du serpent avec lui-meme, deplacement autorise *)
let new_step(pl : t_play) : bool =
  let fstpos : t_position = fst(!(pl.sn)) in
  let dir : t_direction = analyze(fstpos) in
  let (newpos, status) : t_position * t_value = compute_move(fstpos, dir, pl.mat) in
    if status = FRAME
    then 
      (
      set_color(color_of_value(PROBLEM)) ;
      myplot(fstpos.pt.x, fstpos.pt.y) ;
      moveto(mygraphic_x(mymatrix_dx()/4), mygraphic_y(mymatrix_dy() /2)) ;
      draw_string("you fall in the outer space") ;
      true ;
      )
    else 
      if status = SNAKE
      then 
        (
        set_color(color_of_value(PROBLEM)) ;
        myplot(fstpos.pt.x, fstpos.pt.y) ;
        moveto(mygraphic_x(mymatrix_dx()/4), mygraphic_y(mymatrix_dy() /2)) ;
        draw_string("you bit yourself") ;
        true ;
        )
      else
        if status = BONUS
        then
          (
            snake_initial_length(pl);
            false;  
          )
        else  
          (
            move_snake(pl, newpos);
            if(rand_int(0,10) > 5)
            then spawn_bonus(pl)
            else ();
            false;
          )
;;
     
(** {2 Gestion du temps} *)

(** {3 Acceleration} *)
(** effectue une acceleration si les conditions sont remplies *)
let handle_t_acc(t, t_acc, play : float ref * float ref * t_play) : unit = 
  if (!t -. !t_acc) > mydt_acc()
  then 
    (
    play.dt := !(play.dt) *. mydt_ratio() ; 
    t_acc := !t ;
    snake_grow(play);
    ) 
;;

(** {3 Pause entre deux etapes} *)

(** effectue une pause entre deux etapes de jeu. Dans la version qui a ete distribuee initialement (cf. fichier CPsnake_misc.ml), le corps de cette fonction etait une partie de la fonction "simulation" ; c'est maintenant une fonction a part entiere *)
let handle_time(t, dt : float ref * float) : unit = 
  let newt : float ref = ref (Sys.time()) in
    (
    while not((!newt -. !t) > dt)
    do newt := Sys.time() ;
    done ; 
    t := !newt ;
    )
;;

(** {2 Fonction de simulation} *)
(** simule le jeu de serpent : contient principalement la boucle de simulation, dans laquelle se font la gestion du temps (respect de l'intervalle de temps entre deux etapes de jeu, acceleration eventuelle) et le calcul d'une etape de jeu. Par rapport a la version initiale figurant dans le fichier CPsnake_misc.ml, la gestion de l'intervalle de temps entre deux etapes de simulation fait maintenant l'objet d'une fonction a part entiere (handle_time) *)
let simulation() : unit =
  let pl : t_play = init_play() in
  let t : float ref = ref (Sys.time()) and tacc : float ref = ref (Sys.time()) in 
  let thend : bool ref = ref false in
    (
    while not(!thend)
    do
      handle_time(t, !(pl.dt)) ;
      handle_t_acc(t, tacc, pl) ;
      thend := new_step(pl) ;
    done ;
    )
;;

