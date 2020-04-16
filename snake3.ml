#use "CPutil_sn.ml";;

(** Fonction qui réduit le score de 10 
    @param pl le plateau du jeu
    @return le score actualisé
    @author Duc.
    @since 3.0
 *)
let discrease_score ( pl : t_play ) : int =
  if pl.score > 0
  then
  pl.score := (!(pl.score)) - 10;
  (!(pl.score));
;;


(** Fonction qui sert à générer les coordonées du malus sans conflits avec une case existante.
    @param pl le plateau de jeu.
    @return les coordonés du bonus
    @author Duc.
    @since 3.0
 *)
let spawn_malus(pl : t_play) : unit =
  let (rand_x,rand_y : int * int ) = spawn_bonus_coords(pl) in
  (
    set_color(color_of_value(MALUS));
    myplot(rand_x,rand_y);
    pl.mat.(rand_x).(rand_y) <- MALUS;
  )
;;

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
            remove_bonus(pl,newpos);
            snake_initial_length(pl);
            display_score(pl);
            false;  
          )
        else
          if status = MALUS
          then
            (
              remove_bonus(pl,newpos);
              add_snake_new_head(pl,newpos);
              add_snake_new_head(pl,newpos);
              add_snake_new_head(pl,newpos);
              dicrease_score(pl);
            )
          else
          (
            move_snake(pl, newpos);
            if(rand_int(0,10) > 9)
            then spawn_bonus(pl)
            else ();
            false;
          )
;;
